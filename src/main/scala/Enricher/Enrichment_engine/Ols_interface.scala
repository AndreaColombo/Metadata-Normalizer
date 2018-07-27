package Enricher.Enrichment_engine

import java.net.{SocketTimeoutException, URLEncoder}

import Config.config.get_ontologies_by_type
import Enricher.DBCon.{db_handler, default_values, ontology_type, user_feedback_type}
import Enricher.Enrichment_engine.annotator.logger
import Recommender.Ontologies.Parsers.OlsParser.{countWords}
import Utilities.Preprocessing
import Utilities.score_calculator.get_match_score
import com.fasterxml.jackson.core.JsonParseException
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsValue, Json}
import scalaj.http.{Http, HttpOptions}

object Ols_interface {

  def ols_get_status(source: String, iri: String): String = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/"+URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get

  def ols_search_term(term: String, onto: String): (String, String) = {
    val url = "https://www.ebi.ac.uk/ols/api/search"
    val response = Http(url).param("q", term).param("fieldList", "label,short_form,synonym,ontology_name,iri").param("ontology", onto).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
    var max_score = 0
    var result: (String, String)  = ("null", "null")
    var j: JsValue = null

    val logger = LoggerFactory.getLogger(this.getClass)
    try {
      j = (Json.parse(response) \ "response").get("docs")
    }
    catch {
      case e: JsonParseException => logger.info("json parse error",e)
    }

    val service = "Ols"
    val range = j \\ "label"
    for (i <- range.indices) {
      val j2 = j(i)
      val prefLabel = (j2 \ "label").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get
      val ontology_id = (j2 \ "short_form").validate[String].get
      val iri = (j2 \ "iri").validate[String].get
      val score_num = get_match_score(get_score(term, prefLabel), service)

      if (score_num > 6 && score_num > max_score) {
        if(ols_get_status(ontology,iri).contains("200")){
          max_score = score_num
          result = (ontology, ontology_id)
        }
      }
    }
    result
  }

  def ols_get_iri(source: String, code: String): String = {
    var iri_tmp = ""
    try {
      iri_tmp = (Json.parse(Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms").option(HttpOptions.readTimeout(50000)).asString.body) \\ "iri").head.validate[String].get
    }
    catch {
      case e: SocketTimeoutException => logger.info("Read timeout",e.getCause)
    }
    iri_tmp.substring(0,iri_tmp.lastIndexOf("/")+1)+code
  }

  def ols_get_info(source: String, code: String): List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val iri = ols_get_iri(source,code)

    val url = s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/"+URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")
    val status = ols_get_status(source,iri)
    if(status.contains("200")) {
      val response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
      val j = Json.parse(response.body)
      val prefLabel = (j \ "label").validate[String].get
      val ontology = source
      val ontology_id = code
      val description = (j \ "description").validate[List[String]].getOrElse(List("null")).head
      val synonym_l = (j \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",")
      val xref = (j \ "annotation" \ "database_cross_reference").validate[List[String]].getOrElse(List("null"))

      var parents: List[String] = List()
      var part_of: List[String] = List()
      var children: List[String] = List()

      val children_url = url + "/hierarchicalChildren"
      val parents_url = url + "/parents"
      val part_url = url + "/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FBFO_0000050"

      val part_exist = (j \\ "part_of").nonEmpty

      val p_status = Http(parents_url).option(HttpOptions.readTimeout(50000)).asString.header("Status").get
      if (!(j \ "is_root").validate[Boolean].get && p_status.contains("200"))
        ((Json.parse(Http(parents_url).option(HttpOptions.readTimeout(50000)).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => parents :+= a.validate[String].getOrElse("null"))
      else parents = List("null")

      val pp_status = Http(part_url).option(HttpOptions.readTimeout(50000)).asString.header("Status").get
      if (part_exist && pp_status.contains("200"))
        ((Json.parse(Http(part_url).option(HttpOptions.readTimeout(50000)).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => part_of :+= a.validate[String].getOrElse("null"))
      else part_of = List("null")

      val c_status = Http(children_url).option(HttpOptions.readTimeout(50000)).asString.header("Status").get
      if ((j \ "has_children").validate[Boolean].get && c_status.contains("200"))
        ((Json.parse(Http(children_url).option(HttpOptions.readTimeout(50000)).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => children :+= a.validate[String].getOrElse("null"))
      else children = List("null")

      rows :+= List(ontology, ontology_id, prefLabel, xref.mkString(","), synonym, parents.mkString(","), children.mkString(","), part_of.mkString(","),description,iri)
    }
    else {
      logger.info(s"OLS resource not available for $source, $code")
    }
    rows.toList.distinct
  }

  def ols_get_user_feedback(raw_value: String, term_type: String, table_name: String): List[user_feedback_type] = {
    var rows: List[user_feedback_type] = List()
    val parsed = Preprocessing.parse(List(raw_value)).split(",")
    for (value <- parsed) {
      val ontologies = get_ontologies_by_type(term_type)
      val url = "https://www.ebi.ac.uk/ols/api/search"
      val response = Http(url).param("q", value).param("fieldList", "label,short_form,ontology_name").param("ontology", ontologies.mkString(",")).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      val json = (Json.parse(response) \ "response").get("docs")
      for (k <- (json \\ "label").indices) {
        val jj = json(k)
        val label = (jj \ "label").validate[String].get
        val id = (jj \ "short_form").validate[String].get
        val onto = (jj \ "ontology_name").validate[String].get
        val score_num = get_match_score(get_score(raw_value, label), "Ols")
        if (!rows.exists(_.code.get==id) && !db_handler.user_fb_exist(raw_value,onto,id))
          rows :+= user_feedback_type(default_values.int, default_values.bool, table_name, term_type, null, raw_value, Some(value), Some(label), Some(onto), Some(id),Some(ols_get_iri(onto,id)),"ONLINE:LOW  "+score_num.toString)
      }
    }
    rows.distinct
  }

  def ols_get_onto_info(onto: String): ontology_type = {
    var result = ontology_type()
    val url = "https://www.ebi.ac.uk/ols/api/ontologies/"+onto
    val response = Http(url).option(HttpOptions.readTimeout(50000)).asString
    if(response.header("status").get.contains("200")) {
      val json = Json.parse(response.body)
      val source = onto
      val title = (json \ "config").get("title").validate[String].getOrElse(null)
      val description = (json \ "config").get("description").validate[String].getOrElse(null)
      result = ontology_type(source,Some(title),Some(description),Some(url))
    }
    else {
      result = ontology_type("other_link",null,null,null)
    }
    result
  }
  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[String] = List()): String = {
    var score = ""
    val term = termAnnotated.replace("-"," ").map(_.toLower)
    val label = prefLabel.replace("-"," ").map(_.toLower)
    var s = ""
    if (term.length > label.length){
      s = label.r.findAllIn(term).mkString
      println("term "+s)
      if (s.nonEmpty){
        val diff = (countWords(termAnnotated) - countWords(prefLabel))*2
        if (diff > 0)
          score = "PREF - "+diff
        else score = "PREF"
      }
      else score = "LOW"
    }
    else {
      s = term.r.findAllIn(label).mkString
      println("label" +s)
      if (s.nonEmpty){
        val diff = (countWords(prefLabel) - countWords(termAnnotated))*2
        if (diff > 0)
          score = "PREF - "+diff
        else score = "PREF"
      }
      else score = "LOW"
    }
    score
  }
}

package enricher.engine

import java.net.{SocketTimeoutException, URLEncoder}

import config.Config._
import enricher.dbcon.{DbHandler, default_values, expert_choice_type, ontology_type}
import recommender.ontologies.Parsers.OlsParser.countWords
import utilities.{Preprocessing, ScoreCalculator}
import utilities.ScoreCalculator.convert_score_num
import com.fasterxml.jackson.core.JsonParseException
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.{JsValue, Json}
import scalaj.http.{Http, HttpOptions}

case class source_code_label(source: String, code: String, label: String)
case class search_term_result(options: List[source_code_label], score: Double)

object Ols_interface {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def ols_get_status(source: String, iri: String): String = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/"+URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get

  def ols_search_term(term: String, onto: String): search_term_result = {
    val url = "https://www.ebi.ac.uk/ols/api/search"
    val response = Http(url).param("q", term).param("fieldList", "label,short_form,synonym,ontology_name,iri").param("ontology", onto).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body

    var j: JsValue = null
    try {
      j = (Json.parse(response) \ "response").get("docs")
    }
    catch {
      case e: JsonParseException => logger.info("json parse error",e)
    }

    val range = j \\ "label"
    var result = search_term_result(List(),0)

    for (i <- range.indices) {
      val j2 = j(i)
      val prefLabel = (j2 \ "label").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get
      val ontology_id = (j2 \ "short_form").validate[String].get
      val iri = (j2 \ "iri").validate[String].get
      val synonyms = (j2 \ "synonym").validate[List[String]].getOrElse(List())
      val score_num = get_score(term,prefLabel,synonyms)

      //TODO ARIF add check max score also by using match_mode_random
      if (score_num >= get_threshold() && score_num > result.score) {
        if(ols_get_status(ontology,iri).contains("200")){
          result = search_term_result(List(source_code_label(ontology,ontology_id,prefLabel)),score_num)
        }
      }
      else if (!get_search_mode() && score_num >= get_threshold() && score_num == result.score) {
        if(ols_get_status(ontology,iri).contains("200")) {
          val l = result.options :+ source_code_label(ontology,ontology_id,prefLabel)
          result = search_term_result(l,score_num)
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

  //TODO ARIF change list of list of string into list of case classes
  def ols_get_info(source: String, code: String): List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val iri = ols_get_iri(source,code)
    var attempts = 0
    val url = s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/"+URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")
    var status = ols_get_status(source,iri)

    while (!status.contains("200") && attempts <= 5){
      Thread.sleep(10000)
      attempts += 1
      status = ols_get_status(source,iri)
      println("kodio")
    }
    //TODO ARIF no need to run ols_get_status seperately, you can run once then by checking response.code, you can get status as integer
    if(attempts<=5) {
      val response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
      val j = Json.parse(response.body)
      val prefLabel = (j \ "label").validate[String].get
      val ontology = source
      val ontology_id = code
      val description = (j \ "description").validate[List[String]].getOrElse(List("null")).head
      //TODO check all the type of synonyms
      val synonym_l = (j \ "synonyms").validate[List[String]].getOrElse(List("null")).distinct
      val exact_syn = (j \ "annotation" \ "has_exact_synonym").validate[List[String]].getOrElse(List("null")).distinct
      val related_syn = (j \ "annotation" \ "has_related_synonym").validate[List[String]].getOrElse(List("null")).distinct
      //TODO ARIF send distinct syn from the previous class ref: dn_interface line:84

      var synonym_l_fin = (synonym_l ++ exact_syn).filterNot(_.equals("null"))
      if(synonym_l_fin.isEmpty) synonym_l_fin = List("null")

      val synonym = synonym_l_fin.mkString(",")
      val rel_synonym = related_syn.mkString(",")
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

      rows :+= List(ontology, ontology_id, prefLabel, xref.mkString(","), synonym, rel_synonym, parents.mkString(","), children.mkString(","), part_of.mkString(","),description,iri)
    }
    else {
      logger.warn(s"Ols retrieval failed after $attempts attempts")
    }
    rows.toList.distinct
  }

  def ols_get_user_feedback(raw_value: String, term_type: String, table_name: String): List[expert_choice_type] = {
    var rows: List[expert_choice_type] = List()
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
        val synonyms = (jj \ "synonym").validate[List[String]].getOrElse(List())

        val score_num = get_score(raw_value, label,synonyms)
        if (!rows.exists(_.code.get==id) && !DbHandler.user_fb_exist(raw_value,onto,id))
          rows :+= expert_choice_type(default_values.int, default_values.bool, table_name, term_type, null, raw_value, Some(value), Some(label), Some(onto), Some(id),Some(ols_get_iri(onto,id)),"ONLINE:LOW  "+score_num.toString,utilities.Utils.get_timestamp())
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

  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[String] = List()): Double = {
    val pref = config.Config.get_score("pref")
    val syn = config.Config.get_score("syn")
    val modifier = ScoreCalculator.get_words_distance(termAnnotated,prefLabel)
    val score = pref + modifier

    var score_syn = 0.0
    var max_score_syn = 0.0
    for (elem <- synonym_l) {
      val syn_modifier = ScoreCalculator.get_words_distance(termAnnotated,elem)
      score_syn = syn
      if(score_syn>max_score_syn)
        max_score_syn= score_syn + syn_modifier
    }

    math.max(score,max_score_syn)
  }
}
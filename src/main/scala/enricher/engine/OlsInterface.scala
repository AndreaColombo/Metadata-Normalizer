package enricher.engine

import java.net.{SocketTimeoutException, URLEncoder}

import config_pkg.ApplicationConfig.get_ontologies_by_type
import enricher.dbcon.{DbHandler, default_values, expert_choice_type, ontology_type}
import utilities.{Preprocessing, ScoreCalculator}
import com.fasterxml.jackson.core.JsonParseException
import org.slf4j.{Logger, LoggerFactory}
import play.api.libs.json.{JsValue, Json}
import scalaj.http.{Http, HttpOptions}
import RelationType._

case class source_code_iri(source: String, code: String, iri: String)
case class search_term_result(options: List[source_code_iri], score: Double)

object Ols_interface {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  def ols_get_status(source: String, iri: String): String = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/"+URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get

  def ols_search_term(rawValue: RawValue): List[Term] = {
    val url = "https://www.ebi.ac.uk/ols/api/search"
    val ontos = get_ontologies_by_type(rawValue.column)
    val response = Http(url).param("q", rawValue.value).param("fieldList", "iri,short_form,synonym,ontology_name,iri").param("ontology", ontos.mkString(",")).param("rows", "15").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body

    var j: JsValue = null
    try {
      j = (Json.parse(response) \ "response").get("docs")
    }
    catch {
      case e: JsonParseException => logger.info("json parse error",e)
    }

    val range = j \\ "iri"
    var result: List[Term] = List()

    for (i <- range.indices) {
      val j2 = j(i)
      val prefLabel = (j2 \ "iri").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get
      val ontology_id = (j2 \ "short_form").validate[String].get
      val iri = (j2 \ "iri").validate[String].get
      val synonyms = (j2 \ "synonym").validate[List[String]].getOrElse(List())

      result :+= Term(ontology,ontology_id,iri,Some(rawValue))

      //TODO ARIF add check max score also by using match_mode_random
//      if (score_num >= get_threshold() && score_num > result.score) {
//        if(ols_get_status(ontology,iri).contains("200")){
//          result = search_term_result(List(source_code_iri(ontology,ontology_id,prefLabel)),score_num)
//        }
//      }
//      else if (!get_search_mode() && score_num >= get_threshold() && score_num == result.score) {
//        if(ols_get_status(ontology,iri).contains("200")) {
//          val l = result.options :+ source_code_iri(ontology,ontology_id,iri)
//          result = search_term_result(l,score_num)
//        }
//      }
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
  def ols_get_info(source:String,iri: String): Term = {
    var attempts = 0
    val url = s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/"+URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")
    var status = ols_get_status(source,iri)

    var returned: Term = Term(source,"",iri)

    while (!status.contains("200") && attempts <= 5){
      Thread.sleep(10000)
      attempts += 1
      status = ols_get_status(source,iri)
    }
    //TODO ARIF no need to run ols_get_status seperately, you can run once then by checking response.code, you can get status as integer
    if(attempts<=5) {
      val response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
      val j = Json.parse(response.body)
      val prefLabel = (j \ "label").validate[String].get
      val ontology = (j \ "ontology_name").validate[String].get
      val ontology_id = (j \ "short_form").validate[String].get
      val description = (j \ "description").validate[List[String]].getOrElse(List("null")).head
      //TODO check all the type of synonyms
      val synonym_l = (j \ "synonyms").validate[List[String]].getOrElse(List("null")).distinct
      val exact_syn = (j \ "annotation" \ "has_exact_synonym").validate[List[String]].getOrElse(List("null")).distinct
      val related_syn = (j \ "annotation" \ "has_related_synonym").validate[List[String]].getOrElse(List("null")).distinct
      //TODO ARIF send distinct syn from the previous class ref: db_interface line:84

      var synonym_l_fin = (synonym_l ++ exact_syn).filterNot(_.equals("null"))
      if(synonym_l_fin.isEmpty) synonym_l_fin = List("null")

      val synonym = synonym_l.map(f => Synonym(f,SynonymType.SYN))
      val rel_synonym = related_syn.map(f => Synonym(f,SynonymType.RELATED))

      val xref = (j \ "obo_xref").validate[List[JsValue]].getOrElse(List()).map(a =>
        Xref(
          (a \ "database").validate[String].getOrElse("null"),
          (a \ "id").validate[String].get,
          (a \ "url").validate[String].asOpt
        )
      ).filterNot(_.source=="null").map(a =>
        Xref(a.source,a.source+"_"+a.code,a.url)
      )

      synonym.foreach(println)
      val children_url = (j \ "_links" \ "hierarchicalChildren" \ "href").validate[String].getOrElse("null")
      val parents_url = (j \ "_links" \ "parents" \ "href").validate[String].getOrElse("null")
      val part_of_url = (j \ "_links" \ "part_of" \ "href").validate[String].getOrElse("null")

      val parents_tmp = get_relatives(parents_url,RelationType.IS_A)
      val part_of = get_relatives(part_of_url,RelationType.PART_OF)
      val parents = parents_tmp ++ part_of

      val children = get_relatives(children_url, RelationType.PART_OF).map(a =>
        Relation(a.term,get_rel_type(Term(source,"",a.term.right.get),Term(ontology,ontology_id,iri)))
      )

      returned = Term(ontology,ontology_id,iri,None,Some(prefLabel),Some(description),Some(synonym++rel_synonym),Some(xref),Some(parents),Some(children))
//    }
//    else {
//      logger.warn(s"Ols retrieval failed after $attempts attempts")
    }
    returned
  }

  /**
    * Get relationship type between two terms
    * used in children retrieval in which we don't know the relationship type a priori
    * @param child
    * @param parent
    * @return
    */
  def get_rel_type(child: Term, parent: Term): RelationType.ttype = {
    val url = s"https://www.ebi.ac.uk/ols/api/ontologies/${child.source}/terms/"+URLEncoder.encode(URLEncoder.encode(child.iri, "UTF-8"), "UTF-8")
    val response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body

    val j = Json.parse(response)

    val parents_url = (j \ "_links" \ "parents" \ "href").validate[String].getOrElse("null")
    val part_of_url = (j \ "_links" \ "part_of" \ "href").validate[String].getOrElse("null")

    val parents = get_relatives(parents_url,RelationType.IS_A)
    val part_of = get_relatives(part_of_url,RelationType.PART_OF)

    if (parents.exists(a => a.term.right.get == parent.iri)) RelationType.IS_A else RelationType.PART_OF
  }

  /**
    * Get a list of relations based on the url param
    * @param rel_url OLS url of the relatives of the Term
    * @param ttype type of the relationship, IS_A or PART_OF
    * @return a list of relations
    */
  def get_relatives(rel_url: String, ttype: RelationType.ttype): List[Relation] = {
    var parents_tmp: List[Relation] = List()
    if (rel_url != "null"){
       val p_status = Http(rel_url).option(HttpOptions.readTimeout(50000)).asString.header("Status").get
      if(p_status.contains("200")) {
        val parents_json = (Json.parse(Http(rel_url).option(HttpOptions.readTimeout(50000)).asString.body) \ "_embedded").get("terms").validate[List[JsValue]].getOrElse(List())
        parents_tmp = parents_json.map(a =>
          Relation(Right((a \ "iri").validate[String].get), ttype)
        )
      }
    }
    parents_tmp
  }

  def ols_get_user_feedback(raw_value: String, term_type: String, table_name: String): List[expert_choice_type] = {
    var rows: List[expert_choice_type] = List()
    val parsed = Preprocessing.parse(List(raw_value)).split(",")
    for (value <- parsed) {
      val ontologies = get_ontologies_by_type(term_type)
      val url = "https://www.ebi.ac.uk/ols/api/search"
      val response = Http(url).param("q", value).param("fieldList", "iri,short_form,ontology_name").param("ontology", ontologies.mkString(",")).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      val json = (Json.parse(response) \ "response").get("docs")
      for (k <- (json \\ "iri").indices) {
        val jj = json(k)
        val label = (jj \ "iri").validate[String].get
        val id = (jj \ "short_form").validate[String].get
        val onto = (jj \ "ontology_name").validate[String].get
        val synonyms = (jj \ "synonym").validate[List[String]].getOrElse(List())

//        val score_num = get_score(raw_value, label,synonyms)
//        if (!rows.exists(_.code.get==id) && !DbHandler.user_fb_exist(raw_value,onto,id))
//          rows :+= expert_choice_type(default_values.int, default_values.bool, table_name, term_type, null, raw_value, Some(value), Some(label), Some(onto), Some(id),Some(ols_get_iri(onto,id)),"ONLINE:LOW  "+score_num.toString,utilities.Utils.get_timestamp())
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
      val title = (json \ "config_pkg").get("title").validate[String].getOrElse(null)
      val description = (json \ "config_pkg").get("description").validate[String].getOrElse(null)
      result = ontology_type(source,Some(title),Some(description),Some(url))
    }
    else {
      result = ontology_type("other_link",null,null,null)
    }
    result
  }

  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[Synonym] = List()): Double = {
    val pref = config_pkg.ApplicationConfig.get_score("pref")
    val syn = config_pkg.ApplicationConfig.get_score("syn")
    val modifier = ScoreCalculator.get_words_distance(termAnnotated,prefLabel)
    val score = pref + modifier

    var score_syn = 0.0
    var max_score_syn = 0.0
    for (elem <- synonym_l) {
      val syn_modifier = ScoreCalculator.get_words_distance(termAnnotated,elem.label)
      score_syn = syn
      if(score_syn>max_score_syn)
        max_score_syn= score_syn + syn_modifier
    }

    math.max(score,max_score_syn)
  }
}

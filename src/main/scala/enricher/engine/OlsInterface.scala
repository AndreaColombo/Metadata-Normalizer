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
import utilities.Utils.get_timestamp

/**
  * This object contains all of the methods the program uses to interface with OLS REST APIs
  */
object OlsInterface {
  val logger: Logger = LoggerFactory.getLogger(this.getClass)

  /**
    * Return status of a OLS term, usually 200 if term exists, 4xx if term doesn't exist, 500 if currently unavailable
    * @param source Source of the term
    * @param iri Iiri of the term
    * @return
    */
  def ols_get_status(source: String, iri: String): String = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/" + URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.statusLine

  /**
    * Check existence and availability of a term
    * @param source source of the term
    * @param iri iri of the term
    * @return true if term exist
    */
  def ols_exist(source: String, iri: String): Boolean = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/" + URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.is2xx

  /**
    * Retrieve a list of terms, in the form of a tuple (source, code, iri), from the Ols search term endpoint, querying for a specific value, specifying to use only a given set of ontologies
    * @param rawValue An element of case class RawValue containing the value to search for and the term type of the value
    * @return A list of terms with source, code and iri
    */
  def ols_search_term(rawValue: RawValue): List[Term] = {
    logger.info("Retrieving terms for value '"+rawValue.value+"'"+" of '"+rawValue.table+"."+rawValue.column+"'")
    val url = "https://www.ebi.ac.uk/ols/api/search"
    val ontos = get_ontologies_by_type(rawValue.column)
    val response = Http(url).param("q", rawValue.value).param("fieldList", "iri,short_form,synonym,ontology_name,iri").param("ontology", ontos.mkString(",")).param("rows", "15").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000))

    var j: JsValue = null
    try {
      j = (Json.parse(response.asString.body) \ "response").get("docs")
    }
    catch {
      case e: JsonParseException => {
        logger.info("json parse error", e)
        logger.info("Error in parsing JSON response for term search url '" + response.urlBuilder.apply(response))
        logger.info(response.asString.body)
      }
    }

    val range = j \\ "iri"
    var result: List[Term] = List()

    for (i <- range.indices) {
      val j2 = j(i)
      val ontology = (j2 \ "ontology_name").validate[String].get
      val ontology_id = (j2 \ "short_form").validate[String].get
      val iri = (j2 \ "iri").validate[String].get

      val source = ols_get_onto_info(ontology)
      result :+= Term(source, ontology_id, iri)
    }
    if (result.nonEmpty) logger.info("Retrieved following terms for value '"+rawValue.value+"'"+" of '"+rawValue.table+"."+rawValue.column+"'")
    else logger.info("No terms retrieved for value '"+rawValue.value+"'"+" of '"+rawValue.table+"."+rawValue.column+"'")
    result.foreach(a => logger.info(a.toString))
    result
  }

  /**
    * Builds a ols term iri from source and code of the term
    * @param source Source of the term
    * @param code Code of the term
    * @return Ols term iri
    */
  def ols_get_iri(source: String, code: String): String = {
    var iri_tmp = ""
    try {
      iri_tmp = (Json.parse(Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms").option(HttpOptions.readTimeout(50000)).asString.body) \\ "iri").head.validate[String].get
    }
    catch {
      case e: SocketTimeoutException => logger.info("Read timeout", e.getCause)
    }
    iri_tmp.substring(0, iri_tmp.lastIndexOf("/") + 1) + code
  }

  /**
    * Retrieves all info of a term from source and iri
    * @param source Source of the term
    * @param code Code of the term, used only to copy it to the new term returned
    * @param iri Iri of the term
    * @return A new term with all of its field completed
    */
  def ols_get_info(source: String, code: String, iri: String): Term = {
    var attempts = 0
    val url = s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/" + URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")

    val ontology = ols_get_onto_info(source)
    var returned: Term = Term(ontology, code, iri)

    var response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString

    logger.info("Retrieving info for " + code)

    while (!response.is2xx && attempts <= 5) {
      Thread.sleep(10000)
      attempts += 1
      response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
      logger.info("Connecting to ols services attempt " + attempts)
    }
    if (attempts <= 5) {
      logger.info("Connection to ols established")
      val j = Json.parse(response.body)
      val prefLabel = (j \ "label").validate[String].get
      val ontology_id = (j \ "short_form").validate[String].get
      val description = (j \ "description").validate[List[String]].getOrElse(List("null")).head
      val synonym_l = (j \ "synonyms").validate[List[String]].getOrElse(List()).distinct
      val exact_syn = (j \ "annotation" \ "has_exact_synonym").validate[List[String]].getOrElse(List()).distinct
      val related_syn = (j \ "annotation" \ "has_related_synonym").validate[List[String]].getOrElse(List()).distinct

      val synonym_l_fin = (synonym_l ++ exact_syn).distinct

      val synonym = synonym_l_fin.map(f => Synonym(f, SynonymType.SYN))
      val rel_synonym = related_syn.map(f => Synonym(f, SynonymType.RELATED))

      val xref = (j \ "obo_xref").validate[List[JsValue]].getOrElse(List()).map(a =>
        Xref(
          (a \ "database").validate[String].getOrElse("null"),
          (a \ "id").validate[String].get,
          (a \ "url").validate[String].asOpt
        )
      ).filterNot(a => a.code == "null" || a.source == "null").map(a =>
        Xref(a.source, a.source + "_" + a.code, a.url)
      ).distinct

      val children_url = (j \ "_links" \ "children" \ "href").validate[String].getOrElse("null")
      val has_part_url = (j \ "_links" \ "has_part" \ "href").validate[String].getOrElse("null")

      val parents_url = (j \ "_links" \ "parents" \ "href").validate[String].getOrElse("null")
      val part_of_url = (j \ "_links" \ "part_of" \ "href").validate[String].getOrElse("null")

      val parents_tmp = Relation(Right(parents_url), RelationType.IS_A)
      val part_of = Relation(Right(part_of_url), RelationType.PART_OF)
      val parents = List(parents_tmp, part_of)

      val children = List(Relation(Right(children_url), RelationType.IS_A), Relation(Right(has_part_url), PART_OF))

      returned = Term(ontology, ontology_id, iri, None, Some(prefLabel), Some(description), Some(synonym ++ rel_synonym), Some(xref), Some(parents), Some(children))
    }
    else {
      logger.warn(s"Ols retrieval failed after $attempts attempts")
      logger.warn(iri)
    }
    returned
  }

  /**
    * Retrieve all relatives of a term from a ols URL
    * @param relation An element of case class Relation containing the URL and the type of the relation
    * @return A list of terms (source, code, iri) and the relation type
    */
  def get_relatives(relation: Relation): List[Relation] = {
    var rel_tmp: List[Relation] = List()
    val rel_url = relation.term.right.get
    val ttype = relation.ttype
    if (rel_url != "null") {
      logger.info("Relation url not null")
      val response = Http(rel_url).option(HttpOptions.readTimeout(50000)).asString
      if (!response.is2xx) {
        logger.info("Error "+response.code+" in relation retrieval url: "+rel_url)
      }
      else {
        val tmp_rel_json = Json.parse(response.body) \ "_embedded" \ "terms"
        val rel_json =
          if (tmp_rel_json.isDefined)
            tmp_rel_json.validate[List[JsValue]].getOrElse(List())
          else List()

        if(rel_json.isEmpty){
          logger.info("Relation url "+rel_url+" is empty")
        }
        else {logger.info("Relation url ok "+rel_url)}
        rel_tmp = rel_json.map(a => {
          val ontology = (a \ "ontology_name").validate[String].get
          val ontology_id = (a \ "short_form").validate[String].get
          val iri = (a \ "iri").validate[String].get
          val source = ols_get_onto_info(ontology)
          val term = Term(source, ontology_id, iri)
          Relation(Left(term), ttype)
        })
      }
    }
    if(rel_tmp.nonEmpty) logger.info("Retrieved "+rel_tmp.length+" "+ttype)
    rel_tmp
  }

  /**
    * Retrieve infos for user feedback table from Ols search term endpoint
    * This time though the value used for the query is parsed with the NLP parsing engine to clean it
    * By doing so the chance to find a suitable term that matches the value is increased
    * @param rawValue An element of case class RawValue containing the value to search for and the term type of the value
    * @return A list of elements of case class expert_choice
    */
  def ols_get_user_feedback(rawValue: RawValue): List[expert_choice_type] = {
    var rows: List[expert_choice_type] = List()
    val parsed = Preprocessing.parse(List(rawValue.value)).split(",")
    for (value <- parsed) {
      val ontologies = get_ontologies_by_type(rawValue.column)
      val url = "https://www.ebi.ac.uk/ols/api/search"
      val response = Http(url).param("q", value).param("fieldList", "label,iri,short_form,ontology_name").param("ontology", ontologies.mkString(",")).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      val json = (Json.parse(response) \ "response").get("docs")
      for (k <- (json \\ "iri").indices) {
        val jj = json(k)
        val iri = (jj \ "iri").validate[String].get
        val label = (jj \ "label").validate[String].get
        val id = (jj \ "short_form").validate[String].get
        val onto = (jj \ "ontology_name").validate[String].get
        val synonyms = (jj \ "synonym").validate[List[String]].getOrElse(List())
        val score_num = get_score(rawValue.value, label, synonyms)

        //PREVENTS DUPLICATES
        if (!rows.exists(_.code.get == id)) {
          rows :+= expert_choice_type(default_values.int, default_values.bool, rawValue.table, rawValue.column, None, rawValue.value, Some(value), Some(label), Some(onto), Some(id), Some(iri), "ONLINE:LOW  " + score_num.toString, get_timestamp())
        }
      }
    }
    if (rows.nonEmpty) {
      logger.info("Retrieved user feedback info for raw value '"+rawValue.value+"'")
      rows.distinct.filterNot(a => DbHandler.expert_choice_exist(a.raw_value, a.source.get, a.code.get))
    }
    else {
      logger.info("No user feedback info for raw value '"+rawValue.value+"'")
      List(expert_choice_type(default_values.int, default_values.bool, rawValue.table, rawValue.column, None, rawValue.value, None, None, None, None, None, "ONLINE:NONE", get_timestamp()))
    }
  }


  /**
    * Retrieve all info for a given ontology, specifically: title, description and url
    * @param onto The id of and ontology
    * @return An element of case class ontology
    */
  def ols_get_onto_info(onto: String): ontology_type = {
    var result = ontology_type()
    val url = "https://www.ebi.ac.uk/ols/api/ontologies/" + onto
    val response = Http(url).option(HttpOptions.readTimeout(50000)).asString
    if (response.is2xx) {
      val json = Json.parse(response.body)
      val source = onto
      val title = (json \ "config").get("title").validate[String].getOrElse(null)
      val description = (json \ "config").get("description").validate[String].getOrElse(null)
      result = ontology_type(source, Some(title), Some(description), Some(url))
      logger.info("Retrieved info for "+result.toString)
    }
    else {
      result = ontology_type("other_link", null, null, null)
      logger.info("Ontology "+onto+" unknown or doesn't exist")
      logger.info("url: "+url)
    }
    result
  }


  /**
    * Calculate the score of a given annotation
    * @param termAnnotated The value annotated
    * @param prefLabel The preferred label of the annotation
    * @param synonym_l The list of synonym of the annotation
    * @return A Double that is the score of the annotation
    */
  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[String] = List()): Double = {
    val pref = config_pkg.ApplicationConfig.get_match_score("pref")
    val syn = config_pkg.ApplicationConfig.get_match_score("syn")
    val modifier = ScoreCalculator.get_words_distance(termAnnotated, prefLabel)
    val score = pref + modifier

    var score_syn = 0.0
    var max_score_syn = 0.0
    for (elem <- synonym_l) {
      val syn_modifier = ScoreCalculator.get_words_distance(termAnnotated, elem)
      score_syn = syn
      if (score_syn > max_score_syn)
        max_score_syn = score_syn + syn_modifier
    }

    math.max(score, max_score_syn)
  }
}

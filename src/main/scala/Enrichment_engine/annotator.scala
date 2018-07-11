package Enrichment_engine

import java.net.URLEncoder
import java.sql.BatchUpdateException

import DBcon.{gecotest_handler, user_feedback_type}
import Ontologies.Util.OlsParser.get_score
import Utils.Preprocessing
import Utils.score_calculator.get_match_score
import com.fasterxml.jackson.core.JsonParseException
import org.slf4j.LoggerFactory
import play.api.libs.json.{JsValue, Json}
import scalaj.http._

import util.control.Breaks._

object annotator {
  val max_depth = 2

  def get_info(source: String, code: String): List[Map[String, String]] = {
    var result: List[Map[String, String]] = List()
    val tmp = ols_get_info(source,code)
    if (tmp.nonEmpty) {
      val onto = tmp.head(0)
      val parents = tmp.head(5)
      val children = tmp.head(6)

      val desc = get_desc(children, onto, 0)
      val anc = get_hyp(parents, onto, 0)

      if(!gecotest_handler.is_duplicate(onto,tmp.head(1)))
        result :+= Map("source" -> onto, "code" -> tmp.head(1), "label" -> tmp.head(2), "xref" -> tmp.head(3), "syn" -> tmp.head(4), "parents" -> tmp.head(5), "part_of" -> tmp.head(7),"description"->tmp.head(8))

      //IN DESC CI SONO I DISCENDENTI DEL CURRENT TERM
      //IN ANC I SONO GLI ANCESTORS DEL CURRENT TERM

      for (tmp <- anc) {
        if(!gecotest_handler.is_duplicate(tmp._1,tmp._2))
        result :+= Map("source" -> tmp._1, "code" -> tmp._2, "label" -> tmp._3, "xref" -> tmp._4, "syn" -> tmp._5, "parents" -> tmp._6, "part_of" -> tmp._8,"description"->tmp._9)
      }

      for (elem <- desc) {
        if (!gecotest_handler.is_duplicate(elem._1, elem._2))
        result :+= Map("source" -> elem._1, "code" -> elem._2, "label" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8,"description"->elem._9)
      }
    }
    result.distinct
  }

  def search_term(raw_value: String, term_type: String): (String, String) = {
    var res: List[(String, String, String, String, String, String, String, String)] = List()
    var result: (String, String) = ("","")
    val ontos = Utils.Utils.get_ontologies_by_type(term_type).split(",")
    var ok = false
    for (onto <- ontos if !ok){
      val tmp = ols_search_term(raw_value,onto)
      breakable {
        if (tmp._1 == "null")
        break()
        else {
          result = (tmp._1,tmp._2)
          ok = true
        }
      }
    }
    result
  }

  def get_user_feedback(value: String,table_name: String, term_type: String): Unit = {
    var user_feedback: List[user_feedback_type] = List()
    if ({user_feedback = ols_get_user_feedback(value, term_type, table_name); user_feedback.nonEmpty}) {
      try {
        gecotest_handler.user_feedback_insert(user_feedback)
      }
      catch {
        case e: BatchUpdateException => e.getNextException.printStackTrace()
          user_feedback.foreach(println)
          sys.exit(-1)
      }
    }
    else {
      gecotest_handler.user_feedback_insert(List(user_feedback_type(table_name, term_type, null, value, null, null, null, null)))
    }
  }

  def get_desc(children: String, onto: String, depth: Int): List[(String, String, String, String, String, String, String, String,String)] = {
    var result: List[(String, String, String, String, String, String, String, String, String)] = List()
    for (code <- children.split(",")) {
      if (code != "null") {
        val res = ols_get_info(onto,code)
        result :+= (res.head.head, res.head(1), res.head(2), res.head(3), res.head(4), res.head(5), res.head(6), res.head(7),res.head(8))
        val n = depth + 1
        if (n != max_depth)
          result ++= get_desc(res.head(6), res.head(0), n)
        else
          result
      }
    }
    result
  }

  def get_hyp(parents: String, onto: String, depth: Int): List[(String, String, String, String, String, String, String, String,String)] = {
    var result: List[(String, String, String, String, String, String, String, String, String)] = List()
    for (code <- parents.split(",")) {
      if (code != "null") {
        val res = ols_get_info(onto,code)
        result :+= (res.head.head, res.head(1), res.head(2), res.head(3), res.head(4), res.head(5), res.head(6), res.head(7),res.head(8))
        val n = depth + 1
        if (n != max_depth)
        result ++= get_hyp(res.head(5), res.head(0), n)
        else
        result
      }
    }
    result
  }

  def ols_get_info(source: String, code: String): List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val url = s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F" + code
    if(ols_exist(source, code)) {
      val response = Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
      val j = Json.parse(response.body)
      val prefLabel = (j \ "label").validate[String].get
      val ontology = source
      val ontology_id = code
      val description = (j \ "description").validate[List[String]].getOrElse(List("null")).head
      val synonym_l = (j \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",")
      val xref = (j \ "annotation" \ "database_cross_reference").validate[List[String]].getOrElse(List("null"))

      val iri = (j \ "iri").validate[String].get
      val base_url = s"https://www.ebi.ac.uk/ols/api/ontologies/$ontology/terms/" + URLEncoder.encode(URLEncoder.encode(iri, "UTF-8"), "UTF-8")

      var parents: List[String] = List()
      var part_of: List[String] = List()
      var children: List[String] = List()

      val children_url = base_url + "/hierarchicalChildren"
      val parents_url = base_url + "/parents"
      val part_url = base_url + "/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FBFO_0000050"

      val part_exist = (j \\ "part_of").nonEmpty

      val p_status = Http(parents_url).asString.header("Status").get
      if (!(j \ "is_root").validate[Boolean].get && p_status.contains("200"))
      ((Json.parse(Http(parents_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => parents :+= a.validate[String].getOrElse("null"))
      else parents = List("null")

      val pp_status = Http(part_url).asString.header("Status").get
      if (part_exist && pp_status.contains("200"))
      ((Json.parse(Http(part_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => part_of :+= a.validate[String].getOrElse("null"))
      else part_of = List("null")

      val c_status = Http(children_url).asString.header("Status").get
      if ((j \ "has_children").validate[Boolean].get && c_status.contains("200"))
      ((Json.parse(Http(children_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => children :+= a.validate[String].getOrElse("null"))
      else children = List("null")

      rows :+= List(ontology, ontology_id, prefLabel, xref.mkString(","), synonym, parents.mkString(","), children.mkString(","), part_of.mkString(","),description)
    }
    rows.toList.distinct
  }

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

      val score_num = get_match_score(get_score(term, prefLabel), service)

      if (score_num > 6 && score_num > max_score) {
        if(ols_exist(ontology,ontology_id)) {
          max_score = score_num
          result = (ontology, ontology_id)
        }
      }
    }
    result
  }

  def ols_exist(source: String, code: String): Boolean = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F" + code).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get.contains("200")

  def ols_get_user_feedback(raw_value: String, term_type: String, table_name: String): List[user_feedback_type] = {
    var rows: List[user_feedback_type] = List()
    val parsed = Preprocessing.parse(List(raw_value)).split(",")
    for (value <- parsed) {
      val ontologies = Utils.Utils.get_ontologies_by_type(term_type)
      val url = "https://www.ebi.ac.uk/ols/api/search"
      val response = Http(url).param("q", value).param("fieldList", "label,short_form,ontology_name").param("ontology", ontologies).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      val json = (Json.parse(response) \ "response").get("docs")
      for (k <- (json \\ "label").indices) {
        val jj = json(k)
        val label = (jj \ "label").validate[String].get
        val id = (jj \ "short_form").validate[String].get
        val onto = (jj \ "ontology_name").validate[String].get
        if (!rows.exists(_.code.get==id))
          rows :+= user_feedback_type(table_name, term_type, null, raw_value, Some(value), Some(label), Some(onto), Some(id))
      }
    }
    rows.distinct
  }
}
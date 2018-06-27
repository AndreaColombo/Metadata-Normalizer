package Enrichment_engine

import java.net.URLEncoder
import java.sql.BatchUpdateException

import DBcon.gecotest_handler
import Ontologies.Util.OlsParser.get_score
import Utils.Preprocessing
import Utils.score_calculator.get_match_score
import play.api.libs.json.Json
import scalaj.http._
import util.control.Breaks._

object annotator {
  val max_depth = 2
  val url = "https://www.ebi.ac.uk/ols/api/search"

  def get_info(source: String, code: String): List[Map[String, String]] = {
    var result: List[Map[String, String]] = List()
    var res: List[(String, String, String, String, String, String, String, String)] = List()
    val tmp = ols_get_info(code,source)
    if (tmp.nonEmpty) {
      println("good")
      val onto = tmp.head(0)
      val parents = tmp.head(5)
      val children = tmp.head(6)
      res :+= (tmp.head.head, tmp.head(1), tmp.head(2), tmp.head(3), tmp.head(4), tmp.head(5), tmp.head(6), tmp.head(7))

      val desc = get_desc(children, onto, 0)
      val anc = get_hyp(parents, onto, 0)
      result :+= Map("source" -> onto, "code" -> tmp.head(1), "label" -> tmp.head(2), "xref" -> tmp.head(3), "syn" -> tmp.head(4), "parents" -> tmp.head(5), "part_of" -> tmp.head(7))

      //IN DESC CI SONO I DISCENDENTI DEL CURRENT TERM
      //IN ANC I SONO GLI ANCESTORS DEL CURRENT TERM

      for (tmp <- anc) {
        result :+= Map("source" -> tmp._1, "code" -> tmp._2, "label" -> tmp._3, "xref" -> tmp._4, "syn" -> tmp._5, "parents" -> tmp._6, "part_of" -> tmp._8)
      }

      for (elem <- desc)
        result :+= Map("source" -> elem._1, "code" -> elem._2, "label" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8)
    }
    result.distinct
  }

  def search_term(raw_value: String, type_table_name: String, term_type: String): List[Map[String, String]] = {
    var res: List[(String, String, String, String, String, String, String, String)] = List()
    var result: List[Map[String, String]] = List()
    val ontos = Utils.Utils.get_ontologies_by_type(term_type).split(",")

    for (onto <- ontos){
      val response = Http(url).param("q", raw_value).param("fieldList", "label,short_form,synonym,ontology_name,iri").param("ontology", ontos.mkString(",")).param("rows", "5").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      val tmp = ols_search_term(response, raw_value, type_table_name, term_type)
      breakable {
        if (tmp.isEmpty)
          break()
        if (tmp.head.last != "GOOD")
          break()
        else {
          println("good")
          val onto = tmp.head(0)
          val parents = tmp.head(5)
          val children = tmp.head(6)
          res :+= (tmp.head.head, tmp.head(1), tmp.head(2), tmp.head(3), tmp.head(4), tmp.head(5), tmp.head(6), tmp.head(7))

          val desc = get_desc(children, onto, 0)
          val anc = get_hyp(parents, onto, 0)
          result :+= Map("source" -> onto, "code" -> tmp.head(1), "label" -> tmp.head(2), "xref" -> tmp.head(3), "syn" -> tmp.head(4), "parents" -> tmp.head(5), "part_of" -> tmp.head(7))

          //IN DESC CI SONO I DISCENDENTI DEL CURRENT TERM
          //IN ANC I SONO GLI ANCESTORS DEL CURRENT TERM

          for (tmp <- anc) {
            result :+= Map("source" -> tmp._1, "code" -> tmp._2, "label" -> tmp._3, "xref" -> tmp._4, "syn" -> tmp._5, "parents" -> tmp._6, "part_of" -> tmp._8)
          }

          for (elem <- desc) {
            result :+= Map("source" -> elem._1, "code" -> elem._2, "label" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8)
          }
        }
      }
    }
    if(result.isEmpty) {
      var user_feedback: List[List[String]] = List()
      if ({user_feedback = get_user_feedback(raw_value, term_type, type_table_name); user_feedback.nonEmpty}) {
        println("user feedback")
        gecotest_handler.user_feedback_insert(user_feedback)
      }
      else {
        println("not found")
        gecotest_handler.user_feedback_insert(List(List(type_table_name, term_type, raw_value, null, null, null, null)))
      }
    }

    result.distinct
  }

  def get_desc(children: String, onto: String, depth: Int): List[(String, String, String, String, String, String, String, String)] = {
    var result: List[(String, String, String, String, String, String, String, String)] = List()
    for (value <- children.split(",")) {
      if (value != "null") {
        val res = ols_get_info(value, onto)
        result :+= (res.head.head, res.head(1), res.head(2), res.head(3), res.head(4), res.head(5), res.head(6), res.head(7))
        val n = depth + 1
        if (n != max_depth)
          result ++= get_desc(res.head(6), res.head(0), n)
        else
          result
      }
    }
    result
  }

  def get_hyp(parents: String, onto: String, depth: Int): List[(String, String, String, String, String, String, String, String)] = {
    var result: List[(String, String, String, String, String, String, String, String)] = List()
    for (value <- parents.split(",")) {
      if (value != "null") {
        val res = ols_get_info(value, onto)
        result :+= (res.head.head, res.head(1), res.head(2), res.head(3), res.head(4), res.head(5), res.head(6), res.head(7))
        val n = depth + 1
        if (n != max_depth)
          result ++= get_hyp(res.head(5), res.head(0), n)
        else
          result
      }
    }
    result
  }

  def ols_get_info(value: String, onto: String): List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val response = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$onto/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F" + value).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString
    if (!response.header("status").get.contains("200")) {
      println("Error 500")
      //log
    }
    else {
      val j = Json.parse(response.body)
      val prefLabel = (j \ "label").validate[String].get
      val ontology = onto
      val ontology_id = value
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

      rows :+= List(ontology, ontology_id, prefLabel, xref.mkString(","), synonym, parents.mkString(","), children.mkString(","), part_of.mkString(","), "GOOD")
    }
    rows.toList.distinct
  }

  def ols_search_term(response: String, term: String, type_table_name: String, term_type: String): List[List[String]] = {
    var max_score = 0
    var rows: Seq[List[String]] = List()
    val j = (Json.parse(response) \ "response").get("docs")
    val service = "Ols"
    var ok = false

    val range = j \\ "label"
    for (i <- range.indices) {
      val j2 = j(i)
      val prefLabel = (j2 \ "label").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get
      val ontology_id = (j2 \ "short_form").validate[String].get

      val score_num = get_match_score(get_score(term, prefLabel), service)

      if (score_num > 6 && score_num > max_score) {
        ok = true
        max_score = score_num
        rows = ols_get_info(ontology_id, ontology)
      }
    }
    rows.toList
  }

  def ols_exist(source: String, code: String): Boolean = Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$source/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F" + code).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get.contains("200")

  def get_user_feedback(raw_value: String, term_type: String, table_name: String): List[List[String]] = {
    var rows: List[List[String]] = List()
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
        rows :+= List(table_name, term_type, raw_value, value, label, onto, id)
      }
    }
    rows.distinct
  }
}
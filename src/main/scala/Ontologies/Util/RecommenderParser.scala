package Ontologies.Util

import DBcon.query_handler
import play.api.libs.json._
import Utils.Preprocessing.lookup
import scalaj.http.{Http, HttpOptions}


object RecommenderParser {
  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"

  def parse (s: String): List[List[String]] = {
    val j = Json.parse(s)
    var rows: Seq[List[String]] = List()
    val service = "Recommender"
    val l_parsed_value = j \\ "text"
    val l_match_type = j \\ "matchType"
    val l_url = j \\ "self"

    for (i <- l_parsed_value.indices) {

      val parsed_value = l_parsed_value(i).validate[String].get.map(_.toLower)
      val raw_value = lookup(parsed_value)
      val match_type = l_match_type(i).validate[String].get.map(_.toLower)
      //      println(raw_value)
      val url = l_url(i).validate[String].get
      val ontology_raw = get_ontology(url)
      val ontology = ontology_raw.head.map(_.toLower)
      val ontology_id = ontology_raw(1).map(_.toLower)
      val j2 = Json.parse(Http(url).param("apikey", apikey).param("display_context", "false").param("pagesize", "5").header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
      val prefLabel = (j2 \ "prefLabel").validate[String].getOrElse("null").map(_.toLower)
      if (prefLabel != "null") {
        val synonym = (j2 \ "synonym").validate[List[String]].getOrElse(List("null")).mkString(",").map(_.toLower)
        val term_type = query_handler.get_term_type(raw_value)
        //      println(raw_value,parsed_value,ontology,ontology_id,prefLabel,synonym, term_type)
        rows :+= List(service, raw_value, parsed_value, ontology, ontology_id, prefLabel, synonym, "high " + match_type, term_type)
      }
    }
    rows.toList.distinct
  }

  private def get_ontology(s: String): List[String] = {
    val r = "ontologies/([A-Z-a-z-0-9]+)".r
    val s1 = r.findAllIn(s).mkString
    val ontology = s1.substring(s1.lastIndexOf("/")+1)
    val id = s.substring(s.lastIndexOf("%")+3)
    List(ontology,id)
  }
}
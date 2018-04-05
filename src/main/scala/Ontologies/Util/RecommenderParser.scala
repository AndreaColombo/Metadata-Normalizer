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
    val tmp_l_id = j \\ "coverageResult"
    var l_id: Seq[JsValue] = Seq()

    for (j <- tmp_l_id){
      val id = j \\ "@id"
      id.foreach(j => l_id :+= j)
    }
    for (i <- l_parsed_value.indices){

      val parsed_value = l_parsed_value(i).validate[String].get.map(_.toLower)
      val raw_value = lookup(parsed_value)
      val match_type = l_match_type(i).validate[String].get.map(_.toLower)
//      println(raw_value)
      val url = l_url(i).validate[String].get
      val id = l_id(i).validate[String].get
      val ontology_raw = get_ontology(id)
      println(parsed_value)
      println(id)
      println(ontology_raw)
      val ontology = ontology_raw.head.map(_.toLower)
      val ontology_id = ontology_raw(1).map(_.toLower)
      val j2 = Json.parse(Http(url).param("apikey", apikey).param("display_context","false").param("pagesize","5").header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
      val prefLabel = (j2 \ "prefLabel").validate[String].getOrElse(null).map(_.toLower)

      val synonym = (j2 \ "synonym").validate[List[String]].getOrElse(List("null")).mkString(",").map(_.toLower)
      val term_type = query_handler.get_term_type(raw_value)
//      println(raw_value,parsed_value,ontology,ontology_id,prefLabel,synonym, term_type)
      if(prefLabel!=null)
        rows :+= List(service, raw_value, parsed_value, ontology, ontology_id, prefLabel, synonym, "high "+match_type, term_type)
    }
    rows.toList.distinct
  }

  private def get_ontology(s: String): List[String] = {
//    val r = "ontologies/([A-Z-a-z-0-9]+)".r
    val r = "([A-Z-a-z]+)_([0-9]+)".r
    val ontology = r.findAllIn(s).mkString.split("_")
    val id = s.substring(s.lastIndexOf("%")+4).dropRight(1)
    ontology.toList
  }
}
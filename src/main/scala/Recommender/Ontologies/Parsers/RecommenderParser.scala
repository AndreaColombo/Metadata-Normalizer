package Recommender.Ontologies.Parsers

import Recommender.DBCon.db_handler
import Recommender.Ontologies.Parsers.OlsParser.countWords
import play.api.libs.json._
import Utilities.Preprocessing.lookup
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
      var score = ""
      val parsed_value = l_parsed_value(i).validate[String].get.map(_.toLower)
      val raw_value = lookup(parsed_value)
      val match_type = l_match_type(i).validate[String].get.map(_.toLower)
      val url = l_url(i).validate[String].get
      val ontology_raw = get_ontology(url)
      val ontology = ontology_raw.head.map(_.toLower)
      val ontology_id = ontology_raw(1).map(_.toLower)
      val j2 = Json.parse(Http(url).param("apikey", apikey).param("display_context", "false").param("pagesize", "15").header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
      val prefLabel = (j2 \ "prefLabel").validate[String].getOrElse("null").map(_.toLower)
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",").map(_.toLower)

      if(match_type.equals("pref")){
        val s = parsed_value.replace("-"," ").map(_.toLower).r.findAllIn(prefLabel.replace("-"," ").map(_.toLower)).mkString
        val diff = (countWords(prefLabel) - countWords(parsed_value))*2
        if (diff > 0)
          score = "PREF - "+diff
        else score = "PREF"
      }
      else {
        val s2 = parsed_value.replace("-"," ").map(_.toLower).r.findAllIn(synonym.replace("-"," ").map(_.toLower)).mkString
        var syn_found = ""
        var diff_min = 23456
        var s3 = ""

        for (syn <- synonym_l){
          s3 = parsed_value.replace("-"," ").map(_.toLower).r.findAllIn(syn.replace("-"," ").map(_.toLower)).mkString
          if (s3.nonEmpty) {
            val diff = countWords(syn) - countWords(parsed_value)
            if(diff < diff_min){
              diff_min = diff
              syn_found = syn
            }
          }
        }

        val diff = (countWords(syn_found) - countWords(parsed_value))*2
        if (diff > 0)
          score = "SYN - "+diff
        else score = "SYN"
      }
      if (prefLabel != "null") {
        val term_type = ""
        val db_current = db_handler.get_recsys(service)
        val current = List(service,raw_value,parsed_value,ontology.map(_.toLower),ontology_id,prefLabel,synonym,score,term_type)
        if (!rows.exists(p => p.equals(current)))
          rows :+= current
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
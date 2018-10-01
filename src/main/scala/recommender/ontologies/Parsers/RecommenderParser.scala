package recommender.ontologies.Parsers

import config_pkg.ApplicationConfig
import recommender.dbcon.DbHandler
import recommender.ontologies.Parsers.OlsParser.countWords
import play.api.libs.json._
import utilities.Preprocessing.lookup
import utilities.ScoreCalculator
import scalaj.http.{Http, HttpOptions}


object RecommenderParser {
  val apikey = ApplicationConfig.get_bp_apikey()

  def parse (s: String): List[List[String]] = {
    val j = Json.parse(s)
    var rows: Seq[List[String]] = List()
    val service = "recommender"
    val l_parsed_value = j \\ "text"
    val l_match_type = j \\ "matchType"
    val l_url = j \\ "self"
    for (i <- l_parsed_value.indices) {
      val parsed_value = l_parsed_value(i).validate[String].get.map(_.toLower)
      val raw_value = lookup(parsed_value)
      val match_type = l_match_type(i).validate[String].get.map(_.toLower)
      val url = l_url(i).validate[String].get
      val ontology_raw = get_ontology(url)
      val ontology = ontology_raw.head.map(_.toLower)
      val ontology_id = ontology_raw(1).map(_.toLower)
      val j2 = Json.parse(Http(url).param("apikey", apikey).param("display_context", "false").param("pagesize", "15").header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
      val prefLabel = (j2 \ "prefLabel").validate[String].getOrElse("null").map(_.toLower)
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List())
      val synonym = synonym_l.mkString(",").map(_.toLower)

      val score = get_score(parsed_value,match_type,prefLabel,synonym_l)

      if (prefLabel != "null") {
        val term_type = ""
        val db_current = DbHandler.get_recsys(service)
        val current = List(service,raw_value,parsed_value,ontology.map(_.toLower),ontology_id,prefLabel,synonym,score,term_type)
        if (!db_current.exists(p => p.equals(current)))
          rows :+= current
      }
    }
    rows.toList.distinct
  }

  def get_score(term: String, match_type: String, label: String, synonym_l:List[String]): String = {
    var min_syn_diff = Double.NegativeInfinity
    var score = ""
    if(match_type.equals("pref")){
      val diff = ScoreCalculator.get_words_distance(term,label)
      score = "PREF "+diff
    }
    else {
      for (syn <- synonym_l){
        val diff = ScoreCalculator.get_words_distance(term,syn)
        if(diff>min_syn_diff){
          score = "SYN "+diff
          min_syn_diff=diff
        }
      }
    }
    score
  }

  private def get_ontology(s: String): List[String] = {
    val r = "ontologies/([A-Z-a-z-0-9]+)".r
    val s1 = r.findAllIn(s).mkString
    val ontology = s1.substring(s1.lastIndexOf("/")+1)
    val id = s.substring(s.lastIndexOf("%")+3)
    List(ontology,id)
  }
}
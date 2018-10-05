package recommender.ontologies.Parsers

import config_pkg.ApplicationConfig
import recommender.ontologies.Parsers.OlsParser.countWords
import play.api.libs.json._
import utilities.Preprocessing.lookup
import utilities.ScoreCalculator


object BioportalParser {
  val apikey = ApplicationConfig.get_bp_apikey()
  def parse (str: String, term: String): List[List[String]] ={
    var rows:Seq[List[String]] = List()
    val service = "Bioportal"
    val j = (Json.parse(str) \ "collection").get
    val parsed_value = term
    val raw_value = lookup(parsed_value)
    val range = j \\ "prefLabel"
    for (i <- range.indices){
      val j2 = j(i)
      val prefLabel = (j2 \ "prefLabel").validate[String].get
      val matchType = (j2 \ "matchType").validate[String].get
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List())
      val synonym = synonym_l.mkString(",")
      val url = (j2 \ "links" \ "self").validate[String].get

      val score = get_score(parsed_value,matchType,prefLabel,synonym_l)

      val ontology_raw = get_ontology(url)
      val ontology = ontology_raw.head
      val ontology_id = ontology_raw(1)
      val term_type = ""
      val current = List(service,raw_value,parsed_value,ontology.map(_.toLower),ontology_id,prefLabel,synonym,score,term_type)
      if (!rows.exists(p => p.equals(current)))
        rows :+= current
    }
    rows.toList.distinct
  }

  def get_score(term: String, match_type: String, label: String, synonym_l:List[String]): String = {
    var min_syn_diff = Double.NegativeInfinity
    var score = ""
    if(match_type.equals("prefLabel")){
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
    val ontology = r.findAllIn(s).mkString.substring(r.findAllIn(s).mkString.lastIndexOf("/")+1)
    val id = s.substring(s.lastIndexOf("%2")+3)
    List(ontology,id)
  }
}
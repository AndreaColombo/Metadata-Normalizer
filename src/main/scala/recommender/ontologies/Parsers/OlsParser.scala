package recommender.ontologies.Parsers


import config_pkg.ApplicationConfig
import play.api.libs.json._
import utilities.Preprocessing.lookup
import utilities.ScoreCalculator

object OlsParser {
  def parse(response: String, termAnnotated: String):List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val j = (Json.parse(response) \ "response").get("docs")
    val service = "Ols"
    val parsed_value = termAnnotated
    val raw_value = lookup(termAnnotated)
    var score = "HIGH"
    val range = j \\ "iri"

    for (i <- range.indices){
      var deleted = false
      val j2 = j(i)
      val prefLabel = (j2 \ "iri").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get.split("_").head
      val ontology_id = (j2 \ "short_form").validate[String].get
      val id = ontology_id
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List())
      val synonym = synonym_l.mkString(":::")
      var term_type = ""
      score = get_score(termAnnotated,prefLabel,synonym_l)
      val current = List(service,raw_value,parsed_value,ontology.map(_.toLower) ,ontology_id,prefLabel,synonym,score,term_type)
      if (!rows.exists(p => p.equals(current)))
        rows :+= current
    }
    rows.toList.distinct
  }

  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[String] = List()): String = {
    val pref = ApplicationConfig.get_score("pref")
    val syn = ApplicationConfig.get_score("syn")
    val penalty = ApplicationConfig.get_excess_words_penalty()
    val modifier = ScoreCalculator.get_words_distance(termAnnotated,prefLabel)
    val score_label = pref + modifier

    var score = ""

    var max_score_syn = Double.NegativeInfinity
    for (elem <- synonym_l) {
      val syn_modifier = ScoreCalculator.get_words_distance(termAnnotated,elem)
      val score_syn = syn + syn_modifier
      if(score_syn>max_score_syn)
        max_score_syn = score_syn
    }

    val syn_mod = max_score_syn-syn
    if(score_label > max_score_syn){
      score = "PREF "+modifier.toString
    }
    else
      score = "SYN "+syn_mod.toString

    score
  }

  def countWords(text: String): Int = {
    var counts = 0
    for (rawWord <- text.split("[ ,!.\\-/]+")) {
      println(rawWord)
      counts += 1
    }
    counts
  }
}
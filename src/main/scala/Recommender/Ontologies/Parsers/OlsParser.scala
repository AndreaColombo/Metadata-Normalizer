package Recommender.Ontologies.Parsers


import play.api.libs.json._
import Utilities.Preprocessing.lookup

object OlsParser {
  def parse(response: String, termAnnotated: String):List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val j = (Json.parse(response) \ "response").get("docs")
    val service = "Ols"
    val parsed_value = termAnnotated
    val raw_value = lookup(termAnnotated)
    var score = "HIGH"
    val range = j \\ "label"

    for (i <- range.indices){
      var deleted = false
      val j2 = j(i)
      val prefLabel = (j2 \ "label").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get.split("_").head
      val ontology_id = (j2 \ "short_form").validate[String].get
      val id = ontology_id
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",")
      var term_type = ""
      score = get_score(termAnnotated,prefLabel,synonym_l)
      val current = List(service,raw_value,parsed_value,ontology.map(_.toLower) ,ontology_id,prefLabel,synonym,score,term_type)
      if (!rows.exists(p => p.equals(current)))
        rows :+= current
    }
    rows.toList.distinct
  }

  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[String] = List()): String = {
    var score = ""
    val synonym = synonym_l.mkString(",")
    val s = termAnnotated.replace("-"," ").map(_.toLower).r.findAllIn(prefLabel.replace("-"," ").map(_.toLower)).mkString
    val s2 = termAnnotated.replace("-"," ").map(_.toLower).r.findAllIn(synonym.replace("-"," ").map(_.toLower)).mkString
    var s3 = ""
    var syn_found = ""
    var diff_min = 23456

    for (syn <- synonym_l){
      s3 = termAnnotated.replace("-"," ").map(_.toLower).r.findAllIn(syn.replace("-"," ").map(_.toLower)).mkString
      if (s3.nonEmpty) {
        val diff = (countWords(syn) - countWords(termAnnotated))*2
        if(diff < diff_min){
          diff_min = diff
          syn_found = syn
        }
      }
    }
    if (s.nonEmpty){
      val diff = (countWords(prefLabel) - countWords(termAnnotated))*2
      if (diff > 0)
        score = "PREF - "+diff
      else score = "PREF"
    }
    else if (s3.nonEmpty){
      val diff = (countWords(syn_found) - countWords(termAnnotated))*2
      if (diff > 0)
        score = "SYN - "+diff
      else score = "SYN"
    }
    else score = "LOW"
    score
  }

  def countWords(text: String): Int = {
    var counts = 0
    for (rawWord <- text.split("[ ,!.-7/]+")) {
      counts += 1
    }
    counts
  }
}
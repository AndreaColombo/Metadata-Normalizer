package Ontologies.Util

import DBcon.query_handler
import play.api.libs.json._
import Utils.Preprocessing.lookup

import scala.collection.mutable

object OlsParser {

  def parse(response: String, termAnnotated: String):List[List[String]] = {
//    println(termAnnotated)

    var rows: Seq[List[String]] = List()
    val j = (Json.parse(response) \ "response").get("docs")
    val service = "Ols"
    val parsed_value = termAnnotated
    val raw_value = lookup(termAnnotated)
    var score = "HIGH"
//    println(termAnnotated)
    val range = j \\ "label"
//    println(range)
//    println(Json.prettyPrint(Json.parse(response)))
    for (i <- range.indices){
      val j2 = j(i)
      var k = i
      val prefLabel = (j2 \ "label").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get.split("_").head
      val ontology_id = (j2 \ "short_form").validate[String].get
      val id = ontology_id.substring(ontology_id.lastIndexOf("_")+1)
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",")
      val term_type = query_handler.get_term_type(raw_value)
      val s = termAnnotated.replace("-"," ").map(_.toLower).r.findAllIn(prefLabel.replace("-"," ").map(_.toLower)).mkString
      val s2 = termAnnotated.replace("-"," ").map(_.toLower).r.findAllIn(synonym.replace("-"," ").map(_.toLower)).mkString
      var s3 = ""
      var syn_found = ""
      var diff_min = 23456

      for (syn <- synonym_l){
        s3 = termAnnotated.replace("-"," ").map(_.toLower).r.findAllIn(syn.replace("-"," ").map(_.toLower)).mkString
        if (s3.nonEmpty) {
          val diff = countWords(syn) - countWords(termAnnotated)
          if(diff < diff_min){
            diff_min = diff
            syn_found = syn
          }
        }
      }

      if (s.nonEmpty){
        val diff = countWords(prefLabel) - countWords(termAnnotated)
        if (diff > 0)
          score = "PREF - "+diff
        else score = "PREF"
      }
      else if (s3.nonEmpty){
        val diff = countWords(syn_found) - countWords(termAnnotated)
        if (diff > 0)
          score = "SYN - "+diff
        else score = "SYN"
      }
      else score = "LOW"

//      println(raw_value,parsed_value,ontology,id,prefLabel,synonym,score)
      rows:+=List(service,raw_value,parsed_value,ontology.map(_.toLower),id,prefLabel,synonym,score,term_type)
    }
    rows.toList.distinct
  }

  def countWords(text: String): Int = {
    var counts = 0
    for (rawWord <- text.split("[ ,!.-]+")) {
      counts += 1
    }
    counts
  }
}

package Ontologies.Util

import DBcon.query_handler
import Ontologies.Util.OlsParser.countWords
import play.api.libs.json._
import Utils.Preprocessing.lookup


object BioportalParser {
  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
  def parse (str: String, term: String): List[List[String]] ={
//    println(term)
    var rows:Seq[List[String]] = List()
    val service = "Bioportal"
    val j = (Json.parse(str) \ "collection").get
    val parsed_value = term
    val raw_value = lookup(parsed_value)
    var score = ""
    val range = j \\ "prefLabel"
    for (i <- range.indices){
      val j2 = j(i)
      val preflabel = (j2 \ "prefLabel").validate[String].get
      val matchType = (j2 \ "matchType").validate[String].get
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",")
      val url = (j2 \ "links" \ "self").validate[String].get


      if(matchType.equals("prefLabel")){
        val s = term.replace("-"," ").map(_.toLower).r.findAllIn(preflabel.replace("-"," ").map(_.toLower)).mkString
        val diff = (countWords(preflabel) - countWords(term))*2
        if (diff > 0)
          score = "PREF - "+diff
        else score = "PREF"
      }
      else {
        val s2 = term.replace("-"," ").map(_.toLower).r.findAllIn(synonym.replace("-"," ").map(_.toLower)).mkString
        var syn_found = ""
        var diff_min = 23456
        var s3 = ""

        for (syn <- synonym_l){
          s3 = term.replace("-"," ").map(_.toLower).r.findAllIn(syn.replace("-"," ").map(_.toLower)).mkString
          if (s3.nonEmpty) {
            val diff = countWords(syn) - countWords(term)
            if(diff < diff_min){
              diff_min = diff
              syn_found = syn
            }
          }
        }

        val diff = (countWords(syn_found) - countWords(term))*2
        if (diff > 0)
          score = "SYN - "+diff
        else score = "SYN"
      }


      val ontology_raw = get_ontology(url)
      val ontology = ontology_raw.head
      val ontology_id = ontology_raw(1)
      val term_type = query_handler.get_term_type(raw_value)
      rows :+= List(service,raw_value,parsed_value,ontology.map(_.toLower) ,ontology_id,preflabel,synonym,score,term_type)
//      println(rows)
    }
    rows.toList.distinct
  }

  private def get_ontology(s: String): List[String] = {
    val r = "ontologies/([A-Z-a-z-0-9]+)".r
    val ontology = r.findAllIn(s).mkString.substring(r.findAllIn(s).mkString.lastIndexOf("/")+1)
    val id = s.substring(s.lastIndexOf("%2")+3)
    List(ontology,id)
  }
}
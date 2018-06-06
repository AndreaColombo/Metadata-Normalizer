package Ontologies.Util

import java.net.URLEncoder

import DBcon.query_handler
import play.api.libs.json._
import Utils.Preprocessing.lookup
import scalaj.http.Http

import scala.util.control.Breaks._

object OlsParser {

  def parse(response: String, termAnnotated: String, annotator: Boolean=false):List[List[String]] = {
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
      var deleted = false
      val j2 = j(i)
      val prefLabel = (j2 \ "label").validate[String].get
      val ontology = (j2 \ "ontology_name").validate[String].get.split("_").head
      val ontology_id = (j2 \ "short_form").validate[String].get
      val id = ontology_id
      val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List("null"))
      val synonym = synonym_l.mkString(",")
      var term_type = ""
      if (!annotator) {
        term_type = query_handler.get_term_type(raw_value)
      }

      score = get_score(termAnnotated,prefLabel,synonym_l)

      if(annotator){
        if(score == "PREF") { //IF SCORE > THRESHOLD
          val iri = (j2 \ "iri").validate[String].get
          val base_url = s"https://www.ebi.ac.uk/ols/api/ontologies/$ontology/terms/"+URLEncoder.encode(URLEncoder.encode(iri,"UTF-8"),"UTF-8")
          val j3 = Json.parse(Http(base_url).asString.body)
          val j4 = j3
          val xref = (j4 \ "annotation" \ "database_cross_reference").validate[List[String]].getOrElse(List("null"))

          var parents:List[String] = List()
          var part_of:List[String] = List()
          var children:List[String] = List()

          val children_url = base_url +"/hierarchicalChildren"
          val parents_url = base_url+"/parents"
          val part_url = base_url+"/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FBFO_0000050"

          val part_exist = (j4 \\ "part_of").nonEmpty

          if(!(j4 \ "is_root").validate[Boolean].get)
            ((Json.parse(Http(parents_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => parents :+= a.validate[String].getOrElse("null"))
          else parents = List("null")

          if(part_exist)
            ((Json.parse(Http(part_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => part_of :+= a.validate[String].getOrElse("null"))
          else part_of = List("null")


          if((j4 \ "has_children").validate[Boolean].get)
            ((Json.parse(Http(children_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a=> children:+=a.validate[String].getOrElse("null"))
          else children = List("null")

          rows :+= List(ontology, ontology_id, prefLabel, xref.mkString(","), synonym, parents.mkString(","), children.mkString(","), part_of.mkString(","))
        }
      }
      else rows:+=List(service,raw_value,parsed_value,ontology.map(_.toLower),id,prefLabel,synonym,score,term_type)
    }
    rows.toList.distinct
  }

  def parse(response: String):List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val j = Json.parse(response)
    val j2 = j
    val prefLabel = (j2 \ "label").validate[String].get
    val ontology = (j2 \ "ontology_name").validate[String].get.split("_").head
    val ontology_id = (j2 \ "short_form").validate[String].get
    val id = ontology_id
    val synonym_l = (j2 \ "synonym").validate[List[String]].getOrElse(List("null"))
    val synonym = synonym_l.mkString(",")
    val iri = (j2 \ "iri").validate[String].get
    val base_url = s"https://www.ebi.ac.uk/ols/api/ontologies/$ontology/terms/"+URLEncoder.encode(URLEncoder.encode(iri,"UTF-8"),"UTF-8")
    val j3 = Json.parse(Http(base_url).asString.body)
    val j4 = j3
    val xref = (j4 \ "annotation" \ "database_cross_reference").validate[List[String]].getOrElse(List("null"))

    var parents:List[String] = List()
    var part_of:List[String] = List()
    var children:List[String] = List()
    val children_url = base_url +"/hierarchicalChildren"
    val parents_url = base_url+"/parents"
    val part_url = base_url+"/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FBFO_0000050"

    val part_exist = (j4 \\ "part_of").nonEmpty

    if(!(j4 \ "is_root").validate[Boolean].get)
      ((Json.parse(Http(parents_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => parents :+= a.validate[String].getOrElse("null"))
    else parents = List("null")

    if(part_exist)
      ((Json.parse(Http(part_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a => part_of :+= a.validate[String].getOrElse("null"))
    else part_of = List("null")


    if((j4 \ "has_children").validate[Boolean].get)
      ((Json.parse(Http(children_url).asString.body) \ "_embedded").get("terms") \\ "short_form").foreach(a=> children:+=a.validate[String].getOrElse("null"))
    else children = List("null")

    rows :+= List(ontology, ontology_id, prefLabel, xref.mkString(","), synonym, parents.mkString(","), children.mkString(","), part_of.mkString(","))
    rows.toList.distinct
  }


  def get_score(termAnnotated: String, prefLabel: String, synonym_l: List[String]): String = {
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
    for (rawWord <- text.split("[ ,!.-]+")) {
      counts += 1
    }
    counts
  }
}
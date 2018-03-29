package Ontologies.Util

import play.api.libs.json._
import Utils.Preprocessing.lookup

object OlsParser {

  def parse(response: String, termAnnotated: String):List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val j = (Json.parse(response) \ "response").get("docs")
    //    println(Json.prettyPrint(j)),
    val service = "Ols"
    val parsed_value = termAnnotated
    val raw_value = lookup(termAnnotated)
    val score = "HIGH"
    println(termAnnotated)
    val range = j \\ "label"
//    println(range)

    for (i <- range.indices){
      val j2 = j(i)
      val prefLabel = (j2 \ "label").validate[String].get
      println((j2 \ "short_form").get)
      val ontology = (j2 \ "ontology_name").validate[String].get.split("_").head
      val ontology_id = (j2 \ "short_form").validate[String].get
      val id = ontology_id.substring(ontology_id.lastIndexOf("_")+1)
      val synonym = (j2 \ "synonym").validate[List[String]].getOrElse(List("null")).mkString(",")
      println(raw_value,parsed_value,ontology,id,prefLabel,synonym,score)
      rows:+=List(service,raw_value,parsed_value,ontology,id,prefLabel,synonym,score)
    }
    rows.toList.distinct
  }

  private def quotes (string: String): String = string.replaceAll(""""""","")
}

package Ontologies.Util

import play.api.libs.json._

object ZoomaParser {

  def parse (string: String, termAnnotated: String): List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val service = "ZOOMA"
    val parsed_value = termAnnotated
    val j = Json.parse(string)
    val l_terms = j \\ "propertyValue"
    val l_confidence = j \\ "confidence"
    val ontologies = j \\ "semanticTags"

    for (i <- l_terms.indices){
      if(i%2==0) {
        val prefLabel = l_terms(i).toString().replace(""""""","")
        val ontology_raw = ontologies(i).toString().substring(ontologies(i).toString().lastIndexOf('/')+1).dropRight(2)
        val ontology = ontology_raw.split("_").head
        val ontology_id = ontology_raw.split("_")(1)
        val confidence = l_confidence(i/2).toString().replace(""""""","")
        rows :+= Seq(service,"null",parsed_value,ontology,ontology_id,"null",confidence).toList
      }
    }
    rows.toList
  }

}

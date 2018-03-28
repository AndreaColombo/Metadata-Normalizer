package Ontologies.Util

import play.api.libs.json._
import scalaj.http.{Http, HttpOptions}
import Utils.Preprocessing.lookup

object ZoomaParser {

  def parse (string: String, termAnnotated: String): List[List[String]] = {
    var rows: Seq[List[String]] = List()
    val service = "ZOOMA"
    val raw_value = lookup(termAnnotated)
    val parsed_value = termAnnotated
    val j = Json.parse(string)
    val l_terms = j \\ "propertyValue"
    val l_confidence = j \\ "confidence"
    val ontologies = j \\ "semanticTags"
    val links = j \\ "href"
    for (i <- l_terms.indices){
      if(i%2==0) {
        val url = links(i+1).toString().replaceAll(""""""","")
        val j2 = Json.parse(Http(url).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
        val prefLabel = (j2 \\ "label").head.toString.replaceAll(""""""","")
        val synonyms = (j2 \\ "synonyms").head.toString().replaceAll(""""""","").replace("[","").replace("]","")
        val ontology_raw = ontologies(i).toString().substring(ontologies(i).toString().lastIndexOf('/')+1).dropRight(2)
        val ontology = ontology_raw.split("_").head
        val ontology_id = ontology_raw.split("_")(1)
        val confidence = l_confidence(i/2).toString().replace(""""""","")
        rows :+= List(service,raw_value,parsed_value,ontology,ontology_id,prefLabel,synonyms,confidence)
      }
    }
    rows.toList.distinct
    //END PARSE
  }
}

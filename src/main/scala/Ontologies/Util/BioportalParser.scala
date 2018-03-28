package Ontologies.Util

import play.api.libs.json._
import scalaj.http.{Http, HttpOptions}
import Utils.Preprocessing.lookup


object BioportalParser {
  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
  def parse (str: String, term: String): List[List[String]] ={
    var rows:Seq[List[String]] = List()
    val service = "Bioportal"
    val j = Json.parse(str)
    val parsed_value = term
    val raw_value = lookup(parsed_value)
    val l_preflabel = foo(j, "prefLabel")
    val l_synonym = foo(j,"synonym")
    val l_matchtype = j \\ "matchType"
    val l_url = foo(j,"self")
    for (i <- l_preflabel.indices){
      val preflabel = quotes(l_preflabel(i).toString())
      val j2 = Json.parse(Http(quotes(l_url(i))).param("apikey", apikey).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
      var synonym = ""
      try {
        synonym = (j2 \\ "synonym").head.toString().replaceAll(""""""", "").replace("[", "").replace("]", "")
      }
      catch {
        case e: NoSuchElementException => println(parsed_value+"\n"+Json.prettyPrint(j2))
      }
      if(synonym.isEmpty) synonym = "null"
      val ontology_raw = get_ontology(l_url(i).toString())
      val ontology = ontology_raw.head
      val ontology_id = ontology_raw(1)
      val score = "high "+quotes(l_matchtype(i).toString())
      rows :+= List(service,raw_value,parsed_value,ontology,ontology_id,preflabel,synonym,score)
    }
    rows.toList.distinct
  }

  private def quotes (string: String): String = string.replaceAll(""""""","")

  private def get_ontology(s: String): List[String] = {
    val r = "ontologies/([A-Z-a-z-0-9]+)".r
    val ontology = r.findAllIn(s).mkString.substring(r.findAllIn(s).mkString.lastIndexOf("/")+1)
    val id = s.substring(s.lastIndexOf("%2")+3).dropRight(1)
    List(ontology,id)
  }
  private def foo(s: JsValue, term: String): List[String] = {
    val set = s \\ term
    f(set.map(_.toString()).toList)

  }
  private def f(arr: List[String]): List[String] = arr.indices.collect { case i if i % 2 == 0 => arr(i) }.toList
}
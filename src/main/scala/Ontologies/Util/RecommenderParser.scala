package Ontologies.Util

import play.api.libs.json._

import Utils.Preprocessing.lookup
import scalaj.http.{Http, HttpOptions}


object RecommenderParser {
  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"

  def parse (s: String, map: Map[String, Seq[String]]=null): List[List[String]] = {
    val j = Json.parse(s)
    val terms = j \\ "annotations"
    var rows: Seq[List[String]] = List()
    val service = "Recommender"
    val l_parsed_value = j \\ "text"
    val l_match_type = j \\ "matchType"
    val l_url = foo(j, "self")
    for (i <- l_parsed_value.indices){
      val parsed_value = l_parsed_value(i).toString().replaceAll(""""""","").map(_.toLower)
      val match_type = l_match_type(i).toString().replaceAll(""""""","")
      val url = l_url(i).replaceAll(""""""","")
      val ontology_raw = get_ontology(url)
      var ontology = ""
      var ontology_id = ""
      try {
        ontology = ontology_raw.head
        ontology_id = ontology_raw(1)
      }
      catch {
        case e: NoSuchElementException => println(url, ontology_raw)
      }
      var prefLabel = ""
      var synonym = ""
      val j2 = Json.parse(Http(url).param("apikey", apikey).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
      try {
        prefLabel = (j2 \\ "prefLabel").head.toString().replaceAll(""""""", "")
        synonym = (j2 \\ "synonym").head.toString().replaceAll(""""""", "").replace("[", "").replace("]", "")
      }
      catch {
        case e: NoSuchElementException => println(parsed_value+"\n"+Json.prettyPrint(j2))
      }
      if(synonym.isEmpty) synonym = "null"
      val raw_value = lookup(parsed_value)
      rows :+= List(service, raw_value, parsed_value, ontology, ontology_id, prefLabel, synonym, "high "+match_type)
    }
    rows.toList.distinct
  }

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
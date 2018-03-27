package Ontologies.Util

import play.api.libs.json._

import scala.collection.mutable.ListBuffer
import java.lang.Float.parseFloat
import java.net.URLEncoder

import scalaj.http.{Http, HttpOptions}


object RecommenderParser {
  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"

  def parse (s: String, map: Map[String, Seq[String]]=null): List[List[String]] = {
    val j = Json.parse(s)
    val terms = j \\ "annotations"
    var rows: Seq[List[String]] = List()

    for (a <- terms) {
//      println(Json.prettyPrint(a))
      val service = "Recommender"
      val l_parsed_value = a \\ "text"
      val l_match_type = a \\ "matchType"
      val l_ontology_raw = a \\ "@id"
      val l_url = foo(j, "self")
      for (i <- l_parsed_value.indices){
        val parsed_value = l_parsed_value(i).toString().replaceAll(""""""","").map(_.toLower)
        val match_type = l_match_type(i).toString().replaceAll(""""""","")
        val ontology_raw = get_ontology(l_ontology_raw(i).toString())
        println(l_ontology_raw(i))
        val ontology = ontology_raw.head
        val ontology_id = ontology_raw(1)
        val url = l_url(i)
        val j2 = Json.parse(Http(url.replaceAll(""""""","")).params(Seq("apikey"->apikey)).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body)
        val prefLabel = (j2 \\ "prefLabel").head.toString().replaceAll(""""""","")
        var synonym = (j2 \\ "synonym").head.toString().replaceAll(""""""","").replace("[","").replace("]","")
        if(synonym.isEmpty) synonym = "null"
        val default = (-1,"")
        val raw_value = map.find(_._2.contains(parsed_value)).getOrElse(default)._1.toString
        rows :+= List(service, raw_value, parsed_value, ontology, ontology_id, prefLabel, synonym, "high "+match_type)
      }

//      var row = ListBuffer[String]()
//      val l_ontologies = foo(ontologies(i), "acronym")
//      val l_terms = foo(terms(i), "text")
//      val terms_n = l_terms.length
//      val position = i + 1
//      val score = scores(i)
//      var lst = ""
//      l_terms.foreach(lst += _ + "; ")
//      row.append(position.toString,lst.map(_.toLower).replace(""""""","").dropRight(2), (parseFloat(score.toString())*100).toString)
//      lst = ""
//      l_ontologies.foreach(lst += _ + " ")
//      val l = lst.map(_.toLower).replace(""""""","")
//      row.append(lst.map(_.toLower).replace(""""""","").dropRight(1))
//      row.append(terms_n.toString)
//      rows.append(row.toList)
    }

    rows.toList
  }

  private def get_ontology(s: String): List[String] = {
    var a:List[String] = List()
    val type_l = List("biontology", "obolibrary", "identifiers", "nci")

    if (s.contains("bioontology")) {
      val str = s.substring(s.lastIndexOf("y") + 2).dropRight(1)
      a = str.split("/").toList
    }
    else if (s.contains("obolibrary")){
      val str = s.substring(s.lastIndexOf("/")+1).dropRight(1)
      a = str.split("_").toList
    }
    else if (s.contains("identifiers")){
      val str = s.substring(24).dropRight(1)
      a = str.split("/").toList
    }
    else if (s.contains("nci")){
      val str = s.substring(s.lastIndexOf("#")+1).dropRight(1)
      a = List("NCIT", str)
    }
    else if (s.contains("ebi")){
      val str = s.substring(s.lastIndexOf("/")+1).dropRight(1)
      a = str.split("_").toList
    }
    else if (s.contains("orpha")){
      val str = s.substring(s.lastIndexOf("/")+1).dropRight(1)
      a = str.split("_").toList
    }
    else if (s.contains("ccont")){
      val str = s.substring(s.lastIndexOf("#")+1).dropRight(1)
      a = str.split("_").toList
    }
    a
  }

  private def foo(s: JsValue, term: String): List[String] = {
    val set = s \\ term
    f(set.map(_.toString()).toList)

  }
  private def f(arr: List[String]): List[String] = arr.indices.collect { case i if i % 2 == 0 => arr(i) }.toList


}
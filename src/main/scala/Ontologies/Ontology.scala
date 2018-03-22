package Ontologies

import scalaj.http.{Http, HttpOptions}
import Util._

trait Ontology {
  def say()
  def get_results (term: String, raw_value: String): List[List[String]]
}

object Ontology {

  private class Recommender extends Ontology {
    val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
    val url = "http://data.bioontology.org/recommender"

    override def get_results(keywords: String, raw_value: String = ""): List[List[String]] = {
      var lst = ""
      keywords.replace(" ", ",")
      val response = Http(url).params(Seq("apikey" -> apikey, "input" -> keywords, "input_type" -> "2", "output_type" -> "2")).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      RecommenderParser.parse(response)
    }

    override def say(): Unit = println("I am recommender")
  }

  private class Zooma extends Ontology {

    val url = "https://www.ebi.ac.uk/spot/zooma/v2/api/services/annotate"

    override def get_results(keyword: String, raw_value: String): List[List[String]] = {
      keyword.replace(" ", "+")
      ZoomaParser.parse(Http(url).params(Seq("propertyValue"->keyword)).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body,keyword, raw_value)
    }

    override def say(): Unit = println("I am zooma")
  }

  private class Umls extends Ontology {

  def get_results(searchterm: String, raw_value: String): List[List[String]] = {
      val url = "https://uts-ws.nlm.nih.gov/rest/search/current"
      parse(Http(url).params(Seq("string"->searchterm,"ticket"->Auth.getST(Auth.getTGT()))).asString.body)
    }
    def parse(str: String): List[List[String]] = {
      val a = null
      a
    }
    override def say(): Unit = println("I am recommender")
  }

  private class Bioportal extends Ontology {
    val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
    val url = "http://data.bioontology.org/search"
    override def get_results(term: String, raw_value: String): List[List[String]] = {
      val response = Http(url).params(Seq("apikey"->apikey, "input"->term)).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      parse(response)
    }
    def parse(str: String): List[List[String]] = {
      val a = null
      a
    }
    override def say(): Unit = println("I am recommender")
  }

  def apply(s: String): Ontology = {
    if (s.equalsIgnoreCase("bioportal")) new Bioportal
    else  if (s.equalsIgnoreCase("zooma")) new Zooma
    else  if (s.equalsIgnoreCase("umls")) new Umls
    else new Recommender
  }
}

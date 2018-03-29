package Ontologies

import scalaj.http.{Http, HttpOptions}
import Util._

trait Ontology {
  def get_results (term: String): List[List[String]]
}

object Ontology {

  private class Recommender extends Ontology {
    val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
    val url = "http://data.bioontology.org/recommender"

    override def get_results(keywords: String): List[List[String]] = {
      var lst = ""
      keywords.replace(" ", ",")
      val response = Http(url).params(Seq("apikey" -> apikey, "input" -> keywords, "input_type" -> "2", "output_type" -> "2")).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      RecommenderParser.parse(response)
    }
  }

  private class Zooma extends Ontology {
    val url = "https://www.ebi.ac.uk/spot/zooma/v2/api/services/annotate"

    override def get_results(keyword: String): List[List[String]] = {
      keyword.replace(" ", "+")
      ZoomaParser.parse(Http(url).params(Seq("propertyValue"->keyword)).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body,keyword)
    }
  }

  private class Bioportal extends Ontology {
    val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
    val url = "https://data.bioontology.org/search"

    override def get_results(term: String): List[List[String]] = {
      val response = Http(url).params(Seq("apikey"->apikey, "q"->term)).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      BioportalParser.parse(response,term)
    }
  }
  private class Ols extends Ontology {
    val url = "https://www.ebi.ac.uk/ols/api/search"

    override def get_results(term: String): List[List[String]] = {
      val response = Http(url).param("q",term).param("fieldList","label,short_form,synonym,ontology_name").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      OlsParser.parse(response,term)
    }
  }

  def apply(s: String): Ontology = {
    if (s.equalsIgnoreCase("bioportal")) new Bioportal
    else  if (s.equalsIgnoreCase("zooma")) new Zooma
    else if (s.equalsIgnoreCase("ols")) new Ols
    else new Recommender
  }
}

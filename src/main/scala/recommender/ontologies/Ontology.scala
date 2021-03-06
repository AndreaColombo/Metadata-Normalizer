package recommender.ontologies

import config_pkg.ApplicationConfig
import scalaj.http.{Http, HttpOptions}
import Parsers._

trait Ontology {
   //a string of comma separated keywords
  def input (terms: String): List[List[String]]
}

/**
  * This object contains the definition of ontology class used for recommender part: the single ontology class represent the service used
  * Each class contains the public method input used to retrieve the annotations by sending a single or multiple terms to the REST api.
  * The private method get_results calls the api and then calls its parser class to parse the JSON response. JSON parsers are built in order to uniform the output of the various services.
  */
object Ontology {

  private class Recommender extends Ontology {
    private def get_results(keywords: String): List[List[String]] = {
      val apikey = ApplicationConfig.get_bp_apikey()
      val url = "http://data.bioontology.org/recommender"
      val response = Http(url).params(Seq("apikey" -> apikey, "input" -> keywords, "input_type" -> "2", "output_type" -> "2", "display_context"->"false")).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      RecommenderParser.parse(response)
    }

    override def input(terms: String): List[List[String]] = {
      get_results(terms)
    }
  }

  private class Zooma extends Ontology {
    val url = "https://www.ebi.ac.uk/spot/zooma/v2/api/services/annotate"

    private def get_results(keyword: String): List[List[String]] = {
      keyword.replace(" ", "+")
      ZoomaParser.parse(Http(url).params(Seq("propertyValue"->keyword)).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body,keyword)
    }

    override def input(terms: String): List[List[String]] = {
      var result: Seq[List[String]] = Seq()
      val input_l = terms.split(",")
        for (a <- input_l) {
            val tmp = get_results(a)
            tmp.foreach(result:+=_)
        }
      result.toList
    }
  }

  private class Bioportal extends Ontology {
    val apikey = ApplicationConfig.get_bp_apikey()
    val url = "https://data.bioontology.org/search"

    private def get_results(term: String): List[List[String]] = {
      val response = Http(url).params(Seq("apikey"->apikey, "q"->term,"display_links"->"true","display_context"->"false","pagesize"->"15")).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      BioportalParser.parse(response,term)
    }

    override def input(terms: String): List[List[String]] = {
      var result: Seq[List[String]] = List()
      val input_l = terms.split(",")
      for (a <- input_l){
        val tmp = get_results(a)
        tmp.foreach(result:+=_)
      }
      result.toList
    }
  }

  private class Ols extends Ontology {
    val url = "https://www.ebi.ac.uk/ols/api/search"

    private def get_results(term: String): List[List[String]] = {
      val response = Http(url).param("q",term).param("fieldList","iri,short_form,synonym,ontology_name").param("rows","15").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
      OlsParser.parse(response,term)
    }

    override def input(terms: String): List[List[String]] = {
      var result: Seq[List[String]] = List()
      val input_l = terms.split(",")
      for (a <- input_l){
        val tmp = get_results(a)
        tmp.foreach(result:+=_)
      }
      result.toList
    }
  }


  def apply(s: String): Ontology = {
    if (s.equalsIgnoreCase("bioportal")) new Bioportal
    else  if (s.equalsIgnoreCase("zooma")) new Zooma
    else if (s.equalsIgnoreCase("ols")) new Ols
    else new Recommender
  }
}

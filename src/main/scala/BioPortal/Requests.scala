package BioPortal

import scalaj.http._

object Requests {
  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
  val uri = "http://data.bioontology.org/"

  def annotator (txt: String) : String = {
    val url = "annotator"
    var response = Http(uri+url).params(Seq("text"->txt, "apikey"->apikey)).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
    return response
  }

  def recommender (keywords: String): String = {
    val url = "recommender"
    var lst = ""
    //keywords.foreach(lst+=_+",")
    var response = Http(uri+url).params(Seq("apikey" -> apikey, "input" -> keywords, "input_type" -> "2", "output_type" -> "2", "max_elements_set"->"4")).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body

    return response
  }
  def search (term: String): String = {
    val url = "search"
    val response = Http(uri+url).params(Seq("apikey"->apikey, "input"->term)).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
    return response
  }
}

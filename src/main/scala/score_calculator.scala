import DBcon.{db_handler, query_handler}
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}

object score_calculator {

  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
  val url = "http://data.bioontology.org/recommender"

  def get_recommender_score (term: String, onto: String): Double = {
    var score = 0.0

    val params = Seq("apikey" -> apikey, "input" -> term, "input_type" -> "2", "output_type" -> "1", "display_context"->"false","display_links"->"false","ontologies"->onto.map(_.toUpper),
      "wc"->"0","wa"->"0","wd"->"0.4","ws"->"0.6")
    val response = Http(url).params(params).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
    score = (Json.parse(response) \\ "evaluationScore").head.validate[Double].get
    score
  }

  def get_match_score (term: String, service: String): Int = {
    var score = 0
    val matchType = db_handler.get_match_type(term, service)

    if(service.equalsIgnoreCase("zooma")){
      if (matchType.equalsIgnoreCase("HIGH")) score = 5
      else if (matchType.equalsIgnoreCase("GOOD")) score = 4
      else if (matchType.equalsIgnoreCase("MEDIUM")) score = 3
      else if (matchType.equalsIgnoreCase("LOW")) score = 2
     }
    else if (service.equalsIgnoreCase("recommender")) {
      if (matchType.equalsIgnoreCase("high pref")) score = 5
      else score = 3
    }
    else if (service.equalsIgnoreCase("bioportal")){
      if (matchType.equalsIgnoreCase("high prefLabel")) score = 5
      else score = 3
    }
    else if(service.equalsIgnoreCase("ols")){
      if(matchType.startsWith("PREF")){
        if (matchType.contains("-")){
          val lscore = matchType.split("-")
          println(lscore)
          score = 5 - lscore(1).toInt
        }
        else score = 5
      }
      else if (matchType.startsWith("SYN")){
        if(matchType.contains("-")){
          val lscore = matchType.split("-")
          println(lscore)
          score = 3 - lscore(1).toInt
        }
        else score = 3
      }
      else score = 1
    }
    score
  }

  def ontology_score (onto: String, term_type: String): Unit = {

  }

  def calculate_ontology_score(): Unit = {
    var ontologies: Seq[String] = List()
    var terms: Seq[String] = List()
    val term_type = List("tissue", "disease", "cell_line")
    var insert: Seq[List[String]] = List()
    var score: Seq[Double] = List()
    for (a <- term_type) {
      println(a)
      val ontologies = db_handler.get_ontology_by_type(a)
    }
    for (onto <- ontologies){
      val terms = db_handler.get_term_by_ontology(onto)
      for (term <- terms){
        score :+= get_recommender_score(term,onto)
      }
      val average = score.foldLeft((0.0, 1)) ((acc, i) => ((acc._1 + (i - acc._1) / acc._2), acc._2 + 1))._1
      insert :+= List(onto,a,average.toString)

    }
    db_handler.ontology_score_insert(insert.toList)
  }

}

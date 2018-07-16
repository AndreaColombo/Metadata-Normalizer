package Utilities

import DBcon.db_handler
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}

object score_calculator {

  val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
  val url = "http://data.bioontology.org/recommender"

  def get_recommender_score (term: List[String], onto: String): Double = {
    var score = 0.0

    val params = Seq("apikey" -> apikey, "input" -> term.mkString(","), "input_type" -> "2", "output_type" -> "1", "display_context"->"false","display_links"->"false","ontologies"->onto.map(_.toUpper),
      "wc"->"0","wa"->"1","wd"->"0","ws"->"0")
    val response = Http(url).params(params).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
    val j = Json.parse(response) \\ "evaluationScore"
    if(j.nonEmpty)
      score = j.head.validate[Double].get
    score
  }

  def get_match_score (id: Int, service: String): Int = {
    var score = 0
    val matchType = db_handler.get_match_type(id, service)

    if (service.equalsIgnoreCase("zooma")) {
      if (matchType.equalsIgnoreCase("HIGH")) score = 10
      else if (matchType.equalsIgnoreCase("GOOD")) score = 7
      else if (matchType.equalsIgnoreCase("MEDIUM")) score = 5
      else if (matchType.equalsIgnoreCase("LOW")) score = 3
    }
    else {
      if (matchType.startsWith("PREF")) {
        if (matchType.contains("-")) {
          val lscore = matchType.split("-")
          score = 10 - lscore(1).drop(1).toInt
        }
        else score = 10
      }
      else if (matchType.startsWith("SYN")) {
        if (matchType.contains("-")) {
          val lscore = matchType.split("-")
          score = 5 - lscore(1).drop(1).toInt
        }
        else score = 5
      }
      else score = 1
    }
    if(score<0)
      score = 0

    score
  }
  def get_match_score (matchType: String, service: String): Int = {
    var score = 0

    if (service.equalsIgnoreCase("zooma")) {
      if (matchType.equalsIgnoreCase("HIGH")) score = 10
      else if (matchType.equalsIgnoreCase("GOOD")) score = 7
      else if (matchType.equalsIgnoreCase("MEDIUM")) score = 5
      else if (matchType.equalsIgnoreCase("LOW")) score = 3
    }
    else {
      if (matchType.startsWith("PREF")) {
        if (matchType.contains("-")) {
          val lscore = matchType.split("-")
          score = 10 - lscore(1).drop(1).toInt
        }
        else score = 10
      }
      else if (matchType.startsWith("SYN")) {
        if (matchType.contains("-")) {
          val lscore = matchType.split("-")
          score = 5 - lscore(1).drop(1).toInt
        }
        else score = 5
      }
      else score = 1
    }
    if(score<0)
      score = 0

    score
  }

  def calculate_ontology_score(): Unit = {
    var insert: Seq[(String,Double)] = List()
    var score: Seq[Double] = List()
    val onto_recsys = db_handler.get_ontologies()
    println("inizio")
    for (onto <- onto_recsys) {
      val terms = db_handler.get_parsed_by_ontology(onto)
      println(terms.length)
      var recsys_score = 0.0

      recsys_score = get_recommender_score(terms.take(50),onto)

      insert :+= (onto,recsys_score)
    }
    println("ok")
    db_handler.ontology_score_insert(insert)
  }

  def calculate_score(): Unit = {
    val range = db_handler.get_db_lenght()
    for (i <- 1 to range){
      val a = db_handler.get_onto_service_termtype(i)
      val onto = a._1
      val tt = a._3
      val onto_score = db_handler.get_onto_score(onto,tt)
      println(i)
      val match_score = get_match_score(i, a._2)

      val k1 = 2
      val k2 = 2

      var score1 = match_score.toDouble * (k1 + onto_score.toDouble)
      var score2 = match_score.toDouble + (k2 * onto_score.toDouble)

      if (score1<0)
        score1=0

      if (score2<0)
        score2=0

      db_handler.update_score(score1,score2,onto_score.toDouble,match_score,i)

    }
  }


  def calculate_suitability_score(t: String): Double = {
    val ontos = db_handler.get_ontology_by_type(t)

    var suitability = 0.0
    for (o <- ontos) {
      val tmp_coverage = db_handler.get_onto_coverage(o, t)
      val coverage = tmp_coverage._1.toDouble
      val no_annotations = tmp_coverage._2.toInt
      val terms = db_handler.get_term_by_ontology(o,t)
      var matchscore = 0.0
      for (rv<-terms){
        val score = db_handler.get_max_score(rv.replace(""""""","").replace("""\""",""),o).toDouble
        matchscore += score
      }
      suitability = (matchscore / no_annotations) * coverage
      db_handler.update_suitability(suitability, o, t)
    }

    suitability
  }
}
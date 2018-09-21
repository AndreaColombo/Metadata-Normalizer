package Utilities

import Config.config
import Recommender.DBCon.db_handler
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}

object score_calculator {

  val apikey = config.get_bp_apikey()
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


  def splitFunc(in:String): Array[String] = {
    in.toLowerCase.replaceAll("[()\\[\\]{}]","").split("[ ,!.\\-/]+")
  }


  val DUMMY = "DUMMY"

  val penaltyDelete = -config.get_modifier("deletion")
  val penaltyInsert = -config.get_modifier("insertion")
  val penaltyMismatch = -config.get_modifier("mismatch")
  val penaltySwap = -config.get_modifier("swap")
  val penaltyMatch = -config.get_modifier("match")

  def similarity(first: String, second: String): Double = {
    if (first == second)
      penaltyMatch
    else
      penaltyMismatch
  }


  def swap(first: String, second: String, firstPrec: String, secondPrec: String): Double = {
    if (first == secondPrec && firstPrec == second)
      penaltySwap
    else
      Double.NegativeInfinity

  }

  def get_words_distance(term: String, label: String): Double = {
    val rawList = DUMMY ::  splitFunc(term).toList
    val labelList = DUMMY ::  splitFunc(label).toList

    val matrix = Array.ofDim[Double](rawList.length, labelList.length)

    for (i <- rawList.indices) {
      matrix(i)(0) = i * penaltyDelete
    }

    for (j <- labelList.indices) {
      matrix(0)(j) = j * penaltyInsert
    }

    for (i <- 1 until rawList.length) {
      for (j <- 1 until labelList.length) {
        val matchh = matrix(i - 1)(j - 1) + similarity(rawList(i), labelList(j))

        val swapp =
          if (i - 2 >= 0 && j - 2 >= 0)
            matrix(i - 2)(j - 2) + swap(rawList(i), labelList(j), rawList(i - 1), labelList(j - 1))
          else
            Double.NegativeInfinity


        val delete = matrix(i - 1)(j) + penaltyDelete
        val insert = matrix(i)(j - 1) + penaltyInsert
        matrix(i)(j) = List(matchh, swapp, delete, insert).max
      }
    }

    val res = matrix(rawList.length - 1)(labelList.length - 1)
    res
  }

  def convert_score_num(matchType: String, service: String): Double = {
    var score = 0.0

    if (service.equalsIgnoreCase("zooma")) {
      if (matchType.equalsIgnoreCase("HIGH")) score = 10.0
      else if (matchType.equalsIgnoreCase("GOOD")) score = 7.0
      else if (matchType.equalsIgnoreCase("MEDIUM")) score = 5.0
      else if (matchType.equalsIgnoreCase("LOW")) score = 3.0
    }
    else {
      if (matchType.startsWith("PREF")) {
        score = config.get_score("pref")+matchType.split(" ").last.toDouble
      }
      else if (matchType.startsWith("SYN")) {
        score = config.get_score("syn")+matchType.split(" ").last.toDouble
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
      var recsys_score = 0.0

      recsys_score = get_recommender_score(terms.take(50),onto)

      insert :+= (onto,recsys_score)
    }
    println("ok")
    db_handler.ontology_score_insert(insert)
  }

  def calculate_score(): Unit = {
    val range = db_handler.get_db_lenght()
    calculate_ontology_score()
    for (i <- range){
      val tmp = db_handler.get_onto_service_matchtype(i)
      val onto = tmp._1
      val service = tmp._2
      val match_type = tmp._3
      val onto_score = db_handler.get_onto_score(onto)
      println("\r"+i)
      val match_score = convert_score_num(match_type,service)

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
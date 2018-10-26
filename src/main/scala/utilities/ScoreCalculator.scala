package utilities

import config_pkg.ApplicationConfig
import recommender.dbcon.DbHandler
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}

/**
  * This object contains the heuristics to calculate the various scores the program uses in both recommender and enricher
  */
object ScoreCalculator {

  /**
    * Get recommender acceptance score for an ontology
    * @param term list of terms to send recommender for the query
    * @param onto Ontology to calculate the score for
    * @return
    */
  def get_ontology_acceptance_score(term: List[String], onto: String): Double = {
    var score = 0.0
    val apikey: String = ApplicationConfig.get_bp_apikey()
    val url = "http://data.bioontology.org/recommender"
    val params = Seq("apikey" -> apikey, "input" -> term.mkString(","), "input_type" -> "2", "output_type" -> "1", "display_context"->"false","display_links"->"false","ontologies"->onto.map(_.toUpper),
      "wc"->"0","wa"->"1","wd"->"0","ws"->"0")
    val response = Http(url).params(params).header("accept", "text/json").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.body
    val j = Json.parse(response) \\ "evaluationScore"
    if(j.nonEmpty)
      score = j.head.validate[Double].get
    score
  }


  /**
    * Split a text into an array
    * @param in text to split
    * @return
    */
  def splitFunc(in:String): Array[String] = {
    in.toLowerCase.replaceAll("[()\\[\\]{}]","").split("[ ,!.\\-/]+")
  }

  val DUMMY = "DUMMY"

  val penaltyDelete = -ApplicationConfig.get_modifier("deletion")
  val penaltyInsert = -ApplicationConfig.get_modifier("insertion")
  val penaltyMismatch = -ApplicationConfig.get_modifier("mismatch")
  val penaltySwap = -ApplicationConfig.get_modifier("swap")
  val penaltyMatch = -ApplicationConfig.get_modifier("match")

  /**
    * Calculate similarity between two words
    * @param first First word
    * @param second Second word
    * @return
    */
  def similarity(first: String, second: String): Double = {
    if (first == second)
      penaltyMatch
    else
      penaltyMismatch
  }


  /**
    * Calculate swap penalty between two words
    * @param first First word
    * @param second Second word
    * @param firstPrec alò
    * @param secondPrec ie
    * @return
    */
  def swap(first: String, second: String, firstPrec: String, secondPrec: String): Double = {
    if (first == secondPrec && firstPrec == second)
      penaltySwap
    else
      Double.NegativeInfinity
  }

  /**
    * Calculate words distance between two texts, used to calculate the match score
    * @param term Term annotated
    * @param label Label of the annotation
    * @return Words distance in double
    */
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

  /**
    * Convert text score in numeric score
    * @param matchType Text score
    * @param service Service used for the annotation, required because zooma has a different score metrics than the other services
    * @return
    */
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
        score = ApplicationConfig.get_match_score("pref")+matchType.split(" ").last.toDouble
      }
      else if (matchType.startsWith("SYN")) {
        score = ApplicationConfig.get_match_score("syn")+matchType.split(" ").last.toDouble
      }
      else score = 1
    }

    if(score<0)
      score = 0

    score
  }

  /**
    * Calculate acceptance score for every ontology in apiresults
    */
  def calculate_ontology_score(): Unit = {
    var insert: Seq[(String,Double)] = List()
    var score: Seq[Double] = List()
    val onto_recsys = DbHandler.get_ontologies()
    for (onto <- onto_recsys) {
      val terms = DbHandler.get_parsed_by_ontology(onto)
      var recsys_score = 0.0

      recsys_score = get_ontology_acceptance_score(terms.take(3),onto)

      insert :+= (onto,recsys_score)
    }
    DbHandler.ontology_score_insert(insert)
  }

  /**
    * Calculate score for each annotation (row) in apiresults
    */
  def calculate_score(): Unit = {
    val range = DbHandler.get_db_lenght()
    calculate_ontology_score()
    for (i <- range){
      val tmp = DbHandler.get_onto_service_matchtype(i)
      val onto = tmp._1
      val service = tmp._2
      val match_type = tmp._3
      val onto_score = DbHandler.get_onto_score(onto)
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

      DbHandler.update_score(score1,score2,onto_score.toDouble,match_score,i)
    }
  }

  /**
    * Calculate suitability score for each ontology used for the term type t
    * @param t Term type
    * @return
    */
  def calculate_suitability_score(t: String): Double = {
    val ontos = DbHandler.get_ontology_by_type(t)
    var suitability = 0.0
    for (o <- ontos) {
      val tmp_coverage = DbHandler.get_onto_coverage(o, t)
      val coverage = tmp_coverage._1.toDouble
      val no_annotations = tmp_coverage._2.toInt
      val terms = DbHandler.get_term_by_ontology(o,t)
      var matchscore = 0.0
      for (rv<-terms){
        val score = DbHandler.get_max_score(rv.replace(""""""","").replace("""\""",""),o).toDouble
        matchscore += score
      }
      suitability = (matchscore / no_annotations) * coverage
      DbHandler.update_suitability(suitability, o, t)
    }
    suitability
  }
}
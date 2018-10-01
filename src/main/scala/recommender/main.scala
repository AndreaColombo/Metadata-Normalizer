package recommender

import config_pkg.ApplicationConfig
import utilities.ScoreCalculator.{calculate_ontology_score, calculate_score, calculate_suitability_score}
import recommender.dbcon.DbHandler
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}

object main {

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      if(args.head.equals("-h"))
        print_manual()
      else if (args.head.equals("view"))
        DbHandler.create_view()
      else if (args.head.equals("score")) {
        calculate_score()
        val table_l = ApplicationConfig.get_gcm_table_list()
        for (t <- table_l) {
          val column_l = ApplicationConfig.get_termtype_list(t)
          for (col <- column_l) {
            calculate_suitability_score(col)
          }
        }
      }
      else if (args.head.equals("sets")){
        if (args.length == 2){
          if (args(1) == "all"){
            val table_l = ApplicationConfig.get_gcm_table_list()
            for (t <- table_l) {
              val column_l = ApplicationConfig.get_termtype_list(t)
              
              for (col <- column_l) {
                OntologiesSetCalculator.calculate_ontology_set(col)
              }
            }
          }
          else {
            val t = args(1)
            val column_l = ApplicationConfig.get_termtype_list(t)
            for (col <- column_l) {
              OntologiesSetCalculator.calculate_ontology_set(col)
            }
          }
        }
        else {
          val t = args(1)
          val col = args(2)
          OntologiesSetCalculator.calculate_ontology_set(col)
        }
      }
      else if (args.length == 1){
        if (args.head == "all"){
          val table_l = ApplicationConfig.get_gcm_table_list()
          for (t <- table_l) {
            val column_l = ApplicationConfig.get_termtype_list(t)
            for (col <- column_l) {
              DbFiller.fill_db(t, col)
              calculate_suitability_score(col)
            }
          }
        }
        else {
          val t = args(0)
          val column_l = ApplicationConfig.get_termtype_list(t)
          for (col <- column_l) {
            println(col)
            DbFiller.fill_db(t, col)
            calculate_suitability_score(col)
          }
        }
      }
      else {
        val t = args(0)
        val col = args(1)
        DbFiller.fill_db(t, col)
        calculate_suitability_score(col)
      }
    }
  }

  def print_manual(): Unit = {
    println("Program arguments: ")
    println("all \t\t\t\t\t\t\t\t Launch the script for all table and columns" +
      "\n" +
      "<table_name> \t\t\t\t\t\t Launch the script for that specific table " +
      "\n" +
      "<table_name> <column_name> \t\t\t Launch the script for that specific column")
  }

  def update_set_suitability(t: String): Unit = {
    val set_l = DbHandler.get_best_ontos_per_term(t)
    for (set <- set_l) {
      var set_suit = 0.0
      var total_terms: Set[String] = Set()
      println(set)
      val ontos = ""
      for (onto <- ontos.split(",")){
        val suit = DbHandler.get_score_suitability(onto,t)._2
        val terms = DbHandler.get_term_by_ontology(onto,t).toSet
        val termsgood = (total_terms++terms).filterNot(total_terms)
        val weight = suit*termsgood.size
        set_suit += weight
        total_terms = total_terms ++ terms
      }
      set_suit = set_suit / total_terms.size
      DbHandler.update_suitability_sets(set_suit,ontos,t)
    }
  }
}

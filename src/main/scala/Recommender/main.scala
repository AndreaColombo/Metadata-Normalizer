package Recommender

import Config.config
import Utilities.score_calculator.{calculate_suitability_score,calculate_score,calculate_ontology_score}
import Recommender.DBCon.db_handler

object main {

  def main(args: Array[String]): Unit = {
    if (args.nonEmpty) {
      if(args.head.equals("-h"))
        print_manual()
      else if (args.head.equals("score")) {
        calculate_score()
      }
      else if (args.length == 1){
        if (args.head == "all"){
          val table_l = config.get_gcm_table_list()
          for (t <- table_l) {
            val column_l = config.get_termtype_list(t)
            for (col <- column_l) {
              db_filler.fill_db(t, col)
              calculate_suitability_score(col)
            }
//            for (col <- column_l) {
//              ontologies_set_calculator.calculate_ontology_set(col)
//            }
          }
        }
        else {
          val t = args(0)
          val column_l = config.get_termtype_list(t)
          for (col <- column_l) {
            println(col)
            db_filler.fill_db(t, col)
            calculate_suitability_score(col)
          }
//          for (col <- column_l) {
//            ontologies_set_calculator.calculate_ontology_set(col)
//          }
        }
      }
      else {
        val t = args(0)
        val col = args(1)
        db_filler.fill_db(t, col)
        calculate_suitability_score(col)
//        ontologies_set_calculator.calculate_ontology_set(col)
      }
    }
    else print_manual()
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
    val set_l = db_handler.get_best_ontos_per_term(t)
    for (set <- set_l) {
      var set_suit = 0.0
      var total_terms: Set[String] = Set()
      println(set)
      val ontos = ""
      for (onto <- ontos.split(",")){
        val suit = db_handler.get_score_suitability(onto,t)._2
        val terms = db_handler.get_term_by_ontology(onto,t).toSet
        val termsgood = (total_terms++terms).filterNot(total_terms)
        val weight = suit*termsgood.size
        set_suit += weight
        total_terms = total_terms ++ terms
      }
      set_suit = set_suit / total_terms.size
      db_handler.update_suitability_sets(set_suit,ontos,t)
    }
  }
}

package Recommender

import Config.config
import Utilities.score_calculator.{calculate_suitability_score,calculate_score,calculate_ontology_score}
import Recommender.DBCon.db_handler

object main {

  def main(args: Array[String]): Unit = {
    val t = args(0)
    val column_l = config.get_termtype_list(t)
    for (col <- column_l){
      println(col)
      db_filler.fill_db(t,col)
      db_filler.update_db(t,col)
    }
    calculate_ontology_score()
    calculate_score()
    for (col <- column_l){
      calculate_suitability_score(col)
    }
  }
}

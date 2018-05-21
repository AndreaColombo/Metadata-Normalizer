import java.io.File

import DBcon.db_handler
import com.github.tototoshi.csv.CSVWriter

object ontologies_set_calculator {

  val term_type = List("cell_line")

  def calculate_ontology_set(t: String) = {

    var result: Seq[List[String]] = List()
    var score = 0.0
    var suitability = 0.0
    val ontos = db_handler.get_best_onto_per_term(t)
//    val ontos = Seq(("t1","onto1",List("a","b","c")),("t1","onto2",List("b","c","d","e")),("t1","onto3",List("e")),("t1","onto4",List("c","a","d")))
    val a = db_handler.get_nrv(t)
//    val a = 5
    var terms = List("")
    println(t + "\n")
    for (i <- ontos.indices){
      val onto = ontos(i)._2
      val terms1 = db_handler.get_term_by_ontology(onto, t)
//      val terms1 = ontos(i)._3
      val scores = db_handler.get_score_suitability(onto, t)
      val weight1_sc = terms1.length * scores._1
      val weight1_suit = terms1.length * scores._2
      for (j <- i+1 until ontos.length) {
        val onto2 = ontos(j)._2
        val terms2 = db_handler.get_term_by_ontology(onto2, t)
//        val terms2 = ontos(j)._3
        terms = (terms1 ::: terms2).distinct
        val scores = db_handler.get_score_suitability(onto2, t)
        val terms2good = terms.filterNot(terms1.toSet)
        val weight2_sc = terms2good.length * scores._1
        val weight2_suit = terms2good.length * scores._2
        for (k <- j+1 until ontos.length) {
          val onto3 = ontos(k)._2
          val terms3 = db_handler.get_term_by_ontology(onto3, t)
//          val terms3 = ontos(k)._3
          if (terms.equals((terms ::: terms3).distinct)) { //QUI
            val tot_count = terms.length
            val coverage: Double = tot_count.toDouble / a.toDouble
            suitability = (weight1_suit + weight2_suit) / terms.length
            score = (weight1_sc + weight2_sc) / terms.length
            if (coverage > 0.98) {
              var already_present = false
              for (boh2 <- result if already_present == false)
                already_present = boh2.find(ontos => ontos.equals(onto + "," + onto2)).nonEmpty

              if (!already_present) {
                result :+= List(t, onto + "," + onto2, coverage.toString, score.toString, suitability.toString)
              }
            }
          }
          else {
            val terms12 = terms
            terms = (terms ::: terms3).distinct
            val tot_count = terms.length
            val coverage: Double = tot_count.toDouble / a.toDouble
            val scores = db_handler.get_score_suitability(onto3, t)
            val terms3good = terms.filterNot(terms12.toSet)
            val weight3_sc = terms3good.length * scores._1
            val weight3_suit = terms3good.length * scores._2
            suitability = (weight1_suit + weight2_suit + weight3_suit) / terms.length
            score = (weight1_sc + weight2_sc + weight3_sc) / terms.length
            if (coverage > 0.98) {
              result :+= List(t, onto + "," + onto2 + "," + onto3, coverage.toString, score.toString, suitability.toString)
            }
          }
        }
      }
    }
    val f = new File("best_onto_" + t.replace("_","-") + ".csv")
    val writer = CSVWriter.open(f)
    writer.writeAll(result)
//    println("end " + t + " \n")
//    result
  }
}

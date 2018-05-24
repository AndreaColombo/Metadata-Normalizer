import java.io.File

import DBcon.db_handler
import com.github.tototoshi.csv.CSVWriter

object ontologies_set_calculator {

  val term_type = List("cell_line")

  def calculate_ontology_set(t: String) = {

    var result: Seq[List[String]] = List()
    var score1 = 0.0
    var score2 = 0.0
    var suitability = 0.0
    val ontos = db_handler.get_best_onto_per_term(t)
//    val ontos = Seq(("t1","onto1",List("a","b","c")),("t1","onto2",List("b","c","d","e")),("t1","onto3",List("e")),("t1","onto4",List("c","a","d")))
    val a = db_handler.get_nrv(t)
//    val a = 5
    var terms = Set("")
    println(t + "\n")
    for (i <- 0 to 3){
      val onto = ontos(i)._2
      val terms1 = db_handler.get_term_by_ontology(onto, t).toSet
//      val terms1 = ontos(i)._3
      val scores = db_handler.get_score_suitability(onto, t)
      val weight1_sc1 = terms1.size * scores._1
      val weight1_sc2 = terms1.size * scores._2
      val weight1_suit = terms1.size * scores._3
      for (j <- i+1 until ontos.length) {
        val onto2 = ontos(j)._2
        val terms2 = db_handler.get_term_by_ontology(onto2, t).toSet
//        val terms2 = ontos(j)._3
        terms = terms1 ++ terms2
        val scores = db_handler.get_score_suitability(onto2, t)
        val terms2good = terms.filterNot(terms1.toSet)
        val weight2_sc1 = terms2good.size * scores._1
        val weight2_sc2 = terms2good.size * scores._2
        val weight2_suit = terms2good.size * scores._3
        for (k <- j+1 until ontos.length) {
          val onto3 = ontos(k)._2
          val terms3 = db_handler.get_term_by_ontology(onto3, t)
//          val terms3 = ontos(k)._3
          if (terms.equals(terms ++ terms3)) {
            val tot_count = terms.size
            val coverage: Double = tot_count.toDouble / a.toDouble
            suitability = (weight1_suit + weight2_suit) / terms.size
            score1 = (weight1_sc1 + weight2_sc1) / terms.size
            score2 = (weight1_sc2 + weight2_sc2) / terms.size
            if (coverage > 0.98) {
              var already_present = false
              for (boh2 <- result if already_present == false)
                already_present = boh2.find(ontos => ontos.equals(onto + "," + onto2)).nonEmpty

              if (!already_present) {
                result :+= List(t, onto + "," + onto2, coverage.toString, score1.toString, score2.toString, suitability.toString)
              }
            }
          }
          else {
            val terms12 = terms
            terms = terms ++ terms3
            val tot_count = terms.size
            val coverage: Double = tot_count.toDouble / a.toDouble
            val scores = db_handler.get_score_suitability(onto3, t)
            val terms3good = terms.filterNot(terms12.toSet)
            val weight3_sc1 = terms3good.size * scores._1
            val weight3_sc2 = terms3good.size * scores._2
            val weight3_suit = terms3good.size * scores._3
            suitability = (weight1_suit + weight2_suit + weight3_suit) / terms.size
            score1 = (weight1_sc1 + weight2_sc1 + weight3_sc1) / terms.size
            score2 = (weight1_sc2 + weight2_sc2 + weight3_sc2) / terms.size
            if (coverage > 0.98) {
              result :+= List(t, onto + "," + onto2 + "," + onto3, coverage.toString, score1.toString,score2.toString, suitability.toString)
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

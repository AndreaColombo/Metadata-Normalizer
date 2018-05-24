import java.io.File

import DBcon.db_handler
import com.github.tototoshi.csv.CSVWriter
import  util.control.Breaks._

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
    println(t + "\n")


    for (i <- 0 to 3){
      val onto1 = ontos(i)._2
      val terms1 = db_handler.get_term_by_ontology(onto1, t).toSet
      breakable {
        val scores = db_handler.get_score_suitability(onto1, t)
        val weight1_sc1 = terms1.size * scores._1
        val weight1_sc2 = terms1.size * scores._2
        val weight1_suit = terms1.size * scores._3
        if((terms1.size.toDouble / a.toDouble)==1.0){
          val coverage: Double = terms1.size.toDouble / a.toDouble
          result :+= List(t, onto1, coverage.toString, scores._1.toString, scores._2.toString, scores._3.toString)
          break()
        }
        for (j <- i + 1 until 10) {
          val onto2 = ontos(j)._2
          val terms2 = db_handler.get_term_by_ontology(onto2, t).toSet
          breakable {
            val scores = db_handler.get_score_suitability(onto2, t)
            val terms2good = (terms1 ++ terms2).filterNot(terms1)
            val weight2_sc1 = terms2good.size * scores._1
            val weight2_sc2 = terms2good.size * scores._2
            val weight2_suit = terms2good.size * scores._3
            val terms = terms1 ++ terms2
            if (terms1.size.equals(terms.size)) {
              break()
            }
            if((terms.size.toDouble / a.toDouble)==1.0){
              val coverage: Double = terms.size.toDouble / a.toDouble
              suitability = (weight1_suit + weight2_suit) / terms.size
              score1 = (weight1_sc1 + weight2_sc1) / terms.size
              score2 = (weight1_sc2 + weight2_sc2) / terms.size
              result :+= List(t, onto1 + "," + onto2, coverage.toString, score1.toString, score2.toString, suitability.toString)
              break()
            }
            for (k <- j + 1 until 10) {
              val onto3 = ontos(k)._2
              val terms3 = db_handler.get_term_by_ontology(onto3, t)
              breakable {
                val terms12 = terms1 ++ terms2
                val terms = terms12 ++ terms3

                if (terms12.size.equals(terms.size)) {
                  break()
                }

                val terms3good = terms.filterNot(terms12)
                val weight3_sc1 = terms3good.size * scores._1
                val weight3_sc2 = terms3good.size * scores._2
                val weight3_suit = terms3good.size * scores._3

                if((terms.size.toDouble / a.toDouble)==1.0) {
                  val coverage: Double = terms.size.toDouble / a.toDouble
                  suitability = (weight1_suit + weight2_suit + weight3_suit) / terms.size
                  score1 = (weight1_sc1 + weight2_sc1 + weight3_sc1) / terms.size
                  score2 = (weight1_sc2 + weight2_sc2 + weight3_sc2) / terms.size
                  result :+= List(t, onto1+","+onto2+","+onto3, coverage.toString, score1.toString, score2.toString, suitability.toString)
                }
              }
            }
          }
        }
      }
    }
    var result_true: Seq[List[String]] = List()
    for (i <- result.indices){
      val l = result(i)
      var ontos = l(1).split(",").toList
      if(ontos.length>2){
        ontos = ontos.filterNot(a => a == ontos.apply(1))
        var already_present = false
        for (ontos2 <- result if already_present == false)
          if(ontos2(1).split(",").toSeq.equals(ontos))
            already_present=true

        if(!already_present){
          result_true :+= l
        }
      }
      else result_true :+= l
    }
    val f = new File("best_onto_" + t.replace("_","-") + ".csv")
    val writer = CSVWriter.open(f)
    writer.writeAll(result_true)
//    println("end " + t + " \n")
//    result
  }
}

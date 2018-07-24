package Recommender

import Recommender.DBCon.db_handler

import scala.util.control.Breaks._

object ontologies_set_calculator {

  def calculate_ontology_set(t: String): Unit = {

    var result: Seq[(String, String, Double, Double, Double)] = List()
    var score1 = 0.0
    var suitability = 0.0
    val ontos = db_handler.get_best_onto_per_term(t)
    val a = db_handler.get_nrv(t)
    println(t + "\n")

    val threshold = 0.90

    for (i <- 0 until ontos.length/3){
      val onto1 = ontos(i)._2
      val terms1 = db_handler.get_term_by_ontology(onto1, t).toSet
      breakable {

        val scores = db_handler.get_score_suitability(onto1, t)
        val weight1_sc1 = terms1.size * scores._1
        val weight1_suit = terms1.size * scores._2

        //IF ONTO1 COVERS ALL TERMS NO NEED TO GO FURTHER
        if((terms1.size.toDouble / a.toDouble)==1.0){
          val coverage: Double = terms1.size.toDouble / a.toDouble
          result :+= (t, onto1, coverage, scores._1, scores._2)
          break()
        }

        //IF ONTO1 COVERS ENOUGH TERMS SAVE IT
        if((terms1.size.toDouble / a.toDouble)>=threshold){
          val coverage: Double = terms1.size.toDouble / a.toDouble
          result :+= (t, onto1, coverage, weight1_sc1/terms1.size,weight1_suit/terms1.size)
        }

        for (j <- i + 1 until ontos.length) {
          val onto2 = ontos(j)._2
          val terms2 = db_handler.get_term_by_ontology(onto2, t).toSet

          breakable {

            val scores = db_handler.get_score_suitability(onto2, t)
            val terms2good = (terms1 ++ terms2).filterNot(terms1)
            val weight2_sc1 = terms2good.size * scores._1
            val weight2_suit = terms2good.size * scores._2
            val terms = terms1 ++ terms2

            //IF ONTO2 DOESN'T ADD ANYTHING TO ONTO1 CONTINUE TO NEXT ONTOLOGY
            if (terms1.size.equals(terms.size)){
              break()
            }

            //IF (ONTO1,ONTO2) COVER ALL TERMS NO NEED TO GO FURTHER
            if((terms.size.toDouble / a.toDouble)==1.0){
              val coverage: Double = terms.size.toDouble / a.toDouble
              suitability = (weight1_suit + weight2_suit) / terms.size
              score1 = (weight1_sc1 + weight2_sc1) / terms.size
              result :+= (t, onto1 + "," + onto2, coverage, score1, suitability)
              break()
            }

            //IF (ONTO1,ONTO2) COVER ENOUGH TERMS SAVE IT
            if((terms.size.toDouble/a.toDouble)>=threshold){
              val coverage: Double = terms.size.toDouble/a.toDouble
              result :+= (t, onto1+","+onto2, coverage, (weight1_sc1+weight2_sc1)/terms.size,(weight1_suit+weight2_suit)/terms.size)
            }

            for (k <- j + 1 until ontos.length) {
              val onto3 = ontos(k)._2
              val terms3 = db_handler.get_term_by_ontology(onto3, t)

              breakable {

                val terms12 = terms1 ++ terms2
                val terms = terms12 ++ terms3

                //IF ONTO3 DOESN'T ADD ANYTHING TO (ONTO1,ONTO2) CONTINUE TO NEXT ONTOLOGY
                if (terms12.size.equals(terms.size)) {
                  break()
                }

                val terms3good = terms.filterNot(terms12)
                val weight3_sc1 = terms3good.size * scores._1
                val weight3_suit = terms3good.size * scores._2

                //BEING HERE MEANS THAT ONTO3 ADDS SOMETHING USEFUL TO (ONTO1,ONTO2) AND (ONTO1,ONTO2,ONTO3) COVER ENOUGH TERMS
                if((terms.size.toDouble / a.toDouble)>=threshold) {
                  val coverage: Double = terms.size.toDouble / a.toDouble
                  suitability = (weight1_suit + weight2_suit + weight3_suit) / terms.size
                  score1 = (weight1_sc1 + weight2_sc1 + weight3_sc1) / terms.size
                  result :+= (t, onto1+","+onto2+","+onto3, coverage, score1, suitability)
                }
              }
            }
          }
        }
      }
    }

    var result_true: Seq[(String, String, Double, Double, Double)] = List()
    for (i <- result.indices){
      val l = result(i)
      var ontos = l._2.split(",").toList
      if(ontos.length>2){
        ontos = ontos.filterNot(a => a == ontos.apply(1))
        var already_present = false
        for (ontos2 <- result if already_present == false)
          if(ontos2._2.split(",").toSeq.equals(ontos))
            already_present=true

        if(!already_present){
          result_true :+= l
        }
      }
      else result_true :+= l
    }
    db_handler.insert_best_ontos(result)
  }

  def get_set_coverage(ontos: List[String], t: String): Unit = {
    var terms_full: Set[String] = Set()
    val terms1 = db_handler.get_term_by_ontology(ontos.head,t).toSet
    val terms2 = db_handler.get_term_by_ontology(ontos(1),t).toSet
    terms_full = terms1 ++ terms2
    val coverage = terms_full.size.toDouble / db_handler.get_nrv(t).toDouble
    val terms = db_handler.get_term_by_type(t).toSet

    val missing = terms.filterNot(terms_full)

    println()
    println(ontos.head+"\t"+ontos(1)+"\t"+coverage+"\n")
    println(missing.size)
    missing.foreach(println(_))
    println()
  }
}



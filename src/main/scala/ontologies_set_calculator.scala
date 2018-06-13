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
    val a = db_handler.get_nrv(t)
    println(t + "\n")

    val threshold = 0.90

    for (i <- 0 until ontos.length/3){
      val onto1 = ontos(i)._2
      val terms1 = db_handler.get_term_by_ontology(onto1, t).toSet
      breakable {

        val scores = db_handler.get_score_suitability(onto1, t)
        val weight1_sc1 = terms1.size * scores._1
        val weight1_sc2 = terms1.size * scores._2
        val weight1_suit = terms1.size * scores._3

        //IF ONTO1 COVERS ALL TERMS NO NEED TO GO FURTHER
        if((terms1.size.toDouble / a.toDouble)==1.0){
          val coverage: Double = terms1.size.toDouble / a.toDouble
          result :+= List(t, onto1, coverage.toString, scores._1.toString, scores._2.toString, scores._3.toString)
          break()
        }

        //IF ONTO1 COVERS ENOUGH TERMS SAVE IT
        if((terms1.size.toDouble / a.toDouble)>=threshold){
          val coverage: Double = terms1.size.toDouble / a.toDouble
          result :+= List(t, onto1, coverage.toString, (weight1_sc1/terms1.size).toString,(weight1_sc2/terms1.size).toString,(weight1_suit/terms1.size).toString)
        }

        for (j <- i + 1 until ontos.length/3) {
          val onto2 = ontos(j)._2
          val terms2 = db_handler.get_term_by_ontology(onto2, t).toSet

          breakable {

            val scores = db_handler.get_score_suitability(onto2, t)
            val terms2good = (terms1 ++ terms2).filterNot(terms1)
            val weight2_sc1 = terms2good.size * scores._1
            val weight2_sc2 = terms2good.size * scores._2
            val weight2_suit = terms2good.size * scores._3
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
              score2 = (weight1_sc2 + weight2_sc2) / terms.size
              result :+= List(t, onto1 + "," + onto2, coverage.toString, score1.toString, score2.toString, suitability.toString)
              break()
            }

            //IF (ONTO1,ONTO2) COVER ENOUGH TERMS SAVE IT
            if((terms.size.toDouble/a.toDouble)>=threshold){
              val coverage: Double = terms.size.toDouble/a.toDouble
              result :+= List(t, onto1+","+onto2, coverage.toString, ((weight1_sc1+weight2_sc1)/terms.size).toString,((weight1_sc2+weight2_sc2)/terms1.size).toString,((weight1_suit+weight2_suit)/terms.size).toString)
            }

            for (k <- j + 1 until ontos.length/3) {
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
                val weight3_sc2 = terms3good.size * scores._2
                val weight3_suit = terms3good.size * scores._3

                //BEING HERE MEANS THAT ONTO3 ADDS SOMETHING USEFUL TO (ONTO1,ONTO2) AND (ONTO1,ONTO2,ONTO3) COVER ENOUGH TERMS
                if((terms.size.toDouble / a.toDouble)>=threshold) {
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
    writer.writeAll(result_true.distinct)
//    println("end " + t + " \n")
//    result
  }

  def get_set_coverage(ontos: List[String], t: String) = {
    var terms_full: Set[String] = Set()
    val terms1 = db_handler.get_term_by_ontology(ontos(0),t).toSet
    val terms2 = db_handler.get_term_by_ontology(ontos(1),t).toSet
    terms_full = terms1 ++ terms2
    val coverage = terms_full.size.toDouble / db_handler.get_nrv(t).toDouble
    val terms = db_handler.get_term_by_type(t).toSet

    val missing = terms.filterNot(terms_full)

    println()
    println(ontos(0)+"\t"+ontos(1)+"\t"+coverage+"\n")
    println(missing.size)
    missing.foreach(println(_))
    println()
  }

  def cose() = {
    //try {
    //  if (args.nonEmpty) {
    //    if (args.length > 1 && args(1).equalsIgnoreCase("insert")) {
    //      for (t <- term_type) {
    //        val f = new File("best_onto_" + t.replace("_", "-") + ".csv")
    //        val reader = CSVReader.open(f)
    //        val insertvalue = reader.all()
    //        var ok: Seq[(String, String, Double, Double, Double, Double)] = List()
    //        for (l <- insertvalue) {
    //          ok :+= (l(0), l(1), l(2).toDouble, l(3).toDouble, l(4).toDouble, l(5).toDouble)
    //        }
    //        db_handler.insert_best_ontos(ok)
    //      }
    //    }
    //
    //    if (args.length < 2) {
    //      val term_type = m.apply(args(0))
    //      for (t <- term_type) {
    //        ontologies_set_calculator.calculate_ontology_set(t)
    //      }
    //    }
    //  }
    //}
    //    catch {
    //      case e: Exception => println(e)
    //        e.printStackTrace()
    //        e.getCause.printStackTrace()
    //    }

    //  }

    //    if(args.nonEmpty && args(0).equals("get")){
    //      ontologies_set_calculator.get_set_coverage(args.slice(1,3).toList, args(3))
    //    }
    //    else {
    //      for (tt <- m.keys.toList) {
    //        for (t <- m.apply(tt)) {
    //          val onto_sets = db_handler.get_onto_sets(t)
    //          println(t)
    //          for (onto_set <- onto_sets) {
    //            var terms_full: Set[String] = Set()
    //            var acc = 0.0
    //            for (o <- onto_set.split(",")) {
    //              val scores = db_handler.get_score_suitability(o, t)
    //              val terms = db_handler.get_term_by_ontology(o, t).toSet
    //              val termsgood = (terms_full ++ terms).filterNot(terms_full)
    //              val weight_suit = termsgood.size * scores._3
    //              acc += weight_suit
    //              terms_full ++= terms
    //            }
    //            val new_suit = acc / terms_full.size.toDouble
    //
    //            db_handler.update_suitability_sets(new_suit, onto_set, t)
    //          }
    //          val d2 = System.currentTimeMillis()
    //          get_elapsed_time(d1, d2)
    //        }
    //      }
    //    }
  }
}



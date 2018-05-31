import java.io.File
import java.util.{Calendar, Date}

import DBcon._
import com.github.tototoshi.csv._


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"


  override def main(args: Array[String]): Unit = {

    val m = Map("biosample" -> List("disease", "tissue", "cell_line"), "donor" -> List("ethnicity","species"), "item" -> List("platform"), "experiment_type" -> List("technique","target","feature"))//, "container" -> List("annotation"))

    val d1 = System.currentTimeMillis()
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


    for (tt <- m.keys.toList) {
      for (t <- m.apply(tt)) {
        val onto_sets = db_handler.get_onto_sets(t)
        println(t)
        for (onto_set <- onto_sets) {
          var terms_full: Set[String] = Set()
          var acc = 0.0
          for (o <- onto_set.split(",")) {
            val scores = db_handler.get_score_suitability(o, t)
            val terms = db_handler.get_term_by_ontology(o, t).toSet
            val termsgood = (terms_full ++ terms).filterNot(terms_full)
            val weight_suit = termsgood.size * scores._3
            acc += weight_suit
            terms_full ++= terms
          }
          val new_suit = acc/terms_full.size.toDouble

          db_handler.update_suitability_sets(new_suit,onto_set,t)
        }
        val d2 = System.currentTimeMillis()
        get_elapsed_time(d1, d2)
      }
    }

  }
  def get_elapsed_time(d1: Long, d2: Long) = {
    val elapsed:Double = (d2-d1).toDouble / 1000
    val min: Double = (elapsed / 60).intValue()
    val sec: Double = (((elapsed / 60) - min) * 60).intValue
    val millis = ((((elapsed / 60) - min) * 60) - sec) * 1000
    println(min.toInt + ":" + sec.toInt + ":" + millis.toInt)
  }

  def get_timestamp() = {
    val now = Calendar.getInstance()
    println(now.get(Calendar.HOUR_OF_DAY)+":"+now.get(Calendar.MINUTE)+":"+now.get(Calendar.SECOND))
  }
}




import java.io.File
import java.util.{Calendar, Date}

import DBcon._
import Ontologies.Ontology
import Utils.Preprocessing.{lookup, parse}
import com.github.tototoshi.csv._
import score_calculator.get_recommender_score


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"


  override def main(args: Array[String]): Unit = {
   
//      val t = args(0).replace("_","-")
//      val f = new File("best_onto_"+t+".csv")
//      val reader = CSVReader.open(f)
//      val insertvalue = reader.all()
//      var ok: Seq[(String, String, Double, Double, Double)] = List()
//      for (l <- insertvalue){
//        ok :+= (l(0), l(1), l(2).toDouble, l(3).toDouble, l(4).toDouble)
//      }
//      db_handler.insert_best_ontos(ok)
    

    val d1 = System.currentTimeMillis()


    val m = Map("biosample" -> List("disease","tissue","cell_line"),"donor"->List("ethnicity","species"),"item"->List("platform"),"experiment_type"->List("technique","feature","target"),"container"->List("annotation"))

    score_calculator.calculate_ontology_score()
    score_calculator.calculate_score()
    var t = ""
    if(args.nonEmpty) {
      t = args(0)
      val term_type = m.apply(t)
      for (t <- term_type) {
//        score_calculator.calculate_suitability_score(t)
      }
    }
    val d2 = System.currentTimeMillis()


    get_elapsed_time(d1, d2)
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




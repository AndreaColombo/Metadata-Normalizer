import java.io.File
import java.util.Calendar

import DBcon._
import Ontologies.Ontology
import Utils.Preprocessing.{lookup, parse}
import com.github.tototoshi.csv._
import score_calculator.get_recommender_score


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"




  val f = new File("best_ontos_per_type.csv")
  val reader = CSVReader.open(f)
  val insertvalue = reader.all()
  var ok: Seq[(String, String, Double, Double, Double)] = List()

  for (l <- insertvalue){
    ok :+= (l(0), l(1), l(2).toDouble, l(3).toDouble, l(4).toDouble)
  }

  db_handler.insert_best_ontos(ok)


  def get_timestamp() = {
    val now = Calendar.getInstance()
    println(now.get(Calendar.HOUR_OF_DAY)+":"+now.get(Calendar.MINUTE)+":"+now.get(Calendar.SECOND))
  }
}



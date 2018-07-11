import java.util.{Calendar, Date}

import DBcon.gecotest_handler
import Enrichment_engine.{annotator, db_interface, enrichment_engine}
import org.slf4j.LoggerFactory

object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"
  var i = 0
  override def main(args: Array[String]): Unit = {
    gecotest_handler.init()
    println(Config.config.get_termtype_list("biosample"))
    if (args.nonEmpty) {
      if (args(0).equalsIgnoreCase("user") && args(1).equalsIgnoreCase("selection"))
        user_interface.get_user_feedback()
      else {
        gecotest_handler.init()
        enrichment_engine.controller(args(0))
      }
    }
  }

  def get_elapsed_time(d1: Long, d2: Long): Unit = {
    val elapsed:Double = (d2-d1).toDouble / 1000
    val min: Double = (elapsed / 60).intValue()
    val sec: Double = (((elapsed / 60) - min) * 60).intValue
    val millis = ((((elapsed / 60) - min) * 60) - sec) * 1000
    println(min.toInt + ":" + sec.toInt + ":" + millis.toInt)
  }

  def get_timestamp(): Unit = {
    val now = Calendar.getInstance()
    println(now.get(Calendar.HOUR_OF_DAY)+":"+now.get(Calendar.MINUTE)+":"+now.get(Calendar.SECOND))
  }
}
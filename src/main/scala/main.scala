import java.util.{Calendar, Date}

import DBcon.gecotest_handler
import Enrichment_engine.enrichment_engine
import org.apache.logging.log4j.Logger

object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"
  var i = 0

  override def main(args: Array[String]): Unit = {
    var tuple = ("", "")

    if (args.nonEmpty) {
      if (args(0).equalsIgnoreCase("user") && args(1).equalsIgnoreCase("selection"))
        user_selection.get_user_selection()
      else {
        gecotest_handler.init()
        enrichment_engine.controller(args(0))
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
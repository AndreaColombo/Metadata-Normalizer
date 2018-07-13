import java.util.{Calendar, Date}

import Config.config
import DBcon.gecotest_handler
import Enrichment_engine.enrichment_engine
import org.apache.log4j._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"
  val logger = Logger.getLogger(this.getClass)

  var i = 0

  def setup_logger(): Unit = {
    val PATTERN = "%d [%p] - %l %m%n"
    val runId = 0
    val logName = "run_" + runId + "_" + DateTime.now.toString(DateTimeFormat.forPattern("yyyy_MM_dd_HH_mm_ss_SSS")) + ".log"
    val fa2: FileAppender = new FileAppender()
    fa2.setName("FileLogger")
    fa2.setFile(logName)
    fa2.setLayout(new PatternLayout(PATTERN))
    fa2.setThreshold(Level.ERROR)
    fa2.setAppend(true)
    fa2.activateOptions()
    Logger.getLogger("logger").addAppender(fa2)
  }

  override def main(args: Array[String]): Unit = {
    //setup logger
//    setup_logger()
//    val logger = Logger.getLogger(this.getClass)
//    logger.fatal("ciao")
//
//    try {
//      gecotest_handler.init()
//    }
//    catch {
//      case e: Exception => logger.error("error",e.getCause)
//    }

//    println(config.conf.getStringList("db_config.biosample.disease.ontologies"))
    println(config.get_gcm_table_list())
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
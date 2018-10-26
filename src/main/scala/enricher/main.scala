package enricher

import java.util.Calendar

import config_pkg.ApplicationConfig
import enricher.dbcon.DbHandler
import engine.Engine
import org.apache.log4j.{FileAppender, Level, Logger, PatternLayout}
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import org.postgresql.util.PSQLException
import org.slf4j.LoggerFactory
import user_interface.ExpertPreference


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"

  var i = 0

  def setup_logger(): Unit = {
    val PATTERN = "%d [%p] - %l %m%n"
    val logName = "log/lkb_normalizer_"+DateTime.now.toString(DateTimeFormat.forPattern("yyyy_MM_dd_HH_mm_ss_SSS")) + ".log"
    val fa2: FileAppender = new FileAppender()
    fa2.setName("FileLogger")
    fa2.setFile(logName)
    fa2.setLayout(new PatternLayout(PATTERN))
    fa2.setThreshold(Level.DEBUG)
    fa2.setAppend(true)
    fa2.activateOptions()
    Logger.getRootLogger.addAppender(fa2)
  }

  override def main(args: Array[String]): Unit = {
    setup_logger()
    val logger = LoggerFactory.getLogger(this.getClass)
    val start = System.currentTimeMillis
    try {
      //setup logger
      ApplicationConfig.conf.getObject("db_config")
      if (args.nonEmpty) {
        if (args(0).equals("reset")) {
          DbHandler.null_gcm()
          DbHandler.reset_db()
          DbHandler.drop_fk_gcm()
          DbHandler.init()
          DbHandler.create_fk_gcm()
        }
        else if (args.length == 1) {
          if (args.head == "all") {
            val table_l = ApplicationConfig.get_gcm_table_list()
            for (t <- table_l) {
              val column_l = ApplicationConfig.get_termtype_list(t)
              for (col <- column_l) {
                Engine.controller(t,col)
              }
            }
          }
          else {
            val t = args(0)
            val column_l = ApplicationConfig.get_termtype_list(t)
            for (col <- column_l) {
              println(col)
              Engine.controller(t,col)
            }
          }
        }
        else {
          val t = args(0)
          val col = args(1)
          Engine.controller(t,col)
        }
      }
    }catch{
      case e: Exception => logger.error("Error", e)
    }
    val totalTime = System.currentTimeMillis - start
    logger.info("Elapsed time: %1d ms".format(totalTime))
  }
}

package DBcon

import scala.concurrent._
import slick.jdbc.PostgresProfile.api._
import java.io._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory

object query_handler {
  private val parsedConfig = ConfigFactory.parseFile(new File("src/main/scala/DBcon/application.conf"))
  private val conf= ConfigFactory.load(parsedConfig)

  def run(term_type: String): String = {
    val db = Database.forConfig("mydb1", conf)
    var result: Seq[String]= List()

    var q = sql"".as[String]
    if (term_type.equals("cell_line")) {
      q = sql"""
              select distinct cell_line
              from biosample
              where cell_line not like 'null'""".as[String]
    }
    else if (term_type.equals("disease")) {
      q = sql"""
            select distinct disease
            from biosample
            where disease not like 'null'""".as[String]
    }
    else if(term_type.equals("tissue")) {
      q = sql"""
            select distinct tissue
            from biosample
            where tissue not like 'null'""".as[String]
    }
    try {
      val result_future = db.run(q).map(a => println(a))
      Await.result(result_future, Duration.Inf)
    }
    finally db.close()
    result.mkString(",")
  }
}

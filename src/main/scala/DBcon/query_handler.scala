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

  def run_q1(term_type: String): List[String] = {
    val db = Database.forConfig("mydb1", conf)
    var result: Seq[String]= List()
    val t = term_type


    val m = Map("biosample" -> List("disease","tissue","cell_line"),"donor"->List("ethnicity","species"),"item"->List("platform"),"experiment_type"->List("technique","feature","target"),"container"->List("annotation"))

    val default = (-1,"")
    val table = m.find(_._2.contains(t)).getOrElse(default)._1.toString

    val q =
      sql"""select #$t
           from #$table
           where #$t IS NOT NULL
         """.as[String]
    try {
      val result_future = db.run(q).map(_.foreach(a =>
        result:+=a))
      Await.result(result_future, Duration.Inf)
    }
    finally db.close()
    result.toList
  }

    def get_term_type (term: String): String = {

      val db = Database.forConfig("mydb1",conf)
      var term_type = "unknown"
      val query_term = term
      val tissue = sql"""select distinct tissue
                from biosample
                where exists
                (select *
                from biosample
                where tissue ilike $query_term)""".as[String]

      val disease = sql"""select distinct disease
                from biosample
                where exists
                (select *
                from biosample
                where disease ilike $query_term)""".as[String]

      val cell_line = sql"""select distinct cell_line
                from biosample
                where exists
                (select *
                from biosample
                where cell_line ilike $query_term)""".as[String]

      var flag = false
      var tissue_future: Future[Unit] = Future()
      var disease_future: Future[Unit] = Future()
      var cellline_future: Future[Unit] = Future()

      if (!flag) {
        tissue_future = db.run(tissue).map(a =>
          if (a.nonEmpty) {
            term_type = "tissue"
            flag = true
          })
      }
      Await.result(tissue_future, Duration.Inf)

      if (!flag) {
        disease_future = db.run(disease).map(a =>
          if (a.nonEmpty) {
            term_type = "disease"
            flag = true
          })
      }
      Await.result(disease_future,Duration.Inf)

      if (!flag) {
        cellline_future = db.run(cell_line).map(a =>
          if (a.nonEmpty) {
            term_type = "cell_line"
            flag = true
          })
      }
      Await.result(cellline_future,Duration.Inf)

      db.close()

      term_type
  }

}

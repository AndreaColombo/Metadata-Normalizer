package DBcon

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.PostgresProfile.api._
import java.io._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory

import Tables.ApiResults

object db_handler {

  val parsedConfig = ConfigFactory.parseFile(new File("src/main/scala/DBcon/application.conf"))
  val conf = ConfigFactory.load(parsedConfig)
  val db = Database.forConfig("mydb", conf)
  val ApiResults = TableQuery[ApiResults]

  val setup = DBIO.seq(ApiResults.schema.create)

  val setupfuture = db.run(setup)

  def insert (rows: List[List[String]]) = {
    var ok: Seq[(String, String, String, String, String, String, String, String)] = Seq()

    for (l <- rows) {
      ok :+= (l(0),l(1),l(2),l(3),l(4),l(5),l(6),l(7))
    }
    actual_insert(ok)
  }

private def actual_insert(rows: Iterable[(String, String, String, String, String, String, String, String)]) = {
  val insertAction = ApiResults ++= rows
  val insert = db.run(insertAction)
  Await.result(insert, Duration.Inf)
}




}






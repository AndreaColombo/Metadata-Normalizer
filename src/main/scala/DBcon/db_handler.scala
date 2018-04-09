package DBcon

import scala.concurrent._
import slick.jdbc.PostgresProfile.api._
import java.io._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory
import Tables.{ApiResults, OntologyScore}

object db_handler {

  private val parsedConfig = ConfigFactory.parseFile(new File("src/main/scala/DBcon/application.conf"))
  private val conf= ConfigFactory.load(parsedConfig)
  private val db = Database.forConfig("mydb", conf)
  private val ApiResults = TableQuery[ApiResults]
  private val OntologyScore = TableQuery[OntologyScore]

  val setup = DBIO.seq(ApiResults.schema.create)

  private val setupfuture = db.run(setup)

  def insert (rows: List[List[String]]) = {
    var ok: Seq[(String, String, String, String, String, String, String, String, String)] = Seq()

    for (l <- rows) {
      ok :+= (l(0),l(1),l(2),l(3),l(4),l(5),l(6),l(7),l(8))
    }
    actual_insert(ok)
    println("ok insert")
  }

  private def actual_insert(rows: Iterable[(String, String, String, String, String, String, String, String, String)]) = {
    val db = Database.forConfig("mydb", conf)
    val insertAction = ApiResults ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def update_raw_value (rawValue: String, parsed: String) = {
    val db = Database.forConfig("mydb", conf)
    val q =
      sqlu"""update svr.apiresults1
             set raw_value = $rawValue
             where parsed_value = $parsed
          """

    val result_future = db.run(q)
    Await.result(result_future, Duration.Inf)
    db.close()
  }

  def update_term_type (parsedValue: String, term_type: String): Unit = {
    val q =
      sqlu"""update svr.apiresults1
            set term_type = $term_type
         where parsed_value ilike $parsedValue
          """
    val db = Database.forConfig("mydb", conf)
    Await.result(db.run(q), Duration.Inf)
    db.close()
  }

  def get_match_type (parsedValue: String, service: String): String = {
    val db = Database.forConfig("mydb", conf)
    val q =
      sql"""
           select score
           from svr.apiresults1
           where parsed_value ilike $parsedValue
           and service ilike $service
         """.as[String]
    var match_type = ""
    val future_match = db.run(q).map(a => match_type = a.head)

    Await.result(future_match, Duration.Inf)
    db.close()
    match_type
  }

  def ontology_score_insert(rows: List[List[String]]) = {
    val db = Database.forConfig("mydb", conf)
    var ok: Seq[(String,String,String)] = List()
    for (l<- rows){
      ok :+= (l(0), l(1), l(2))
    }
    val insertaction = OntologyScore ++= ok
    Await.result(db.run(insertaction),Duration.Inf)
    db.close()
  }

  def get_ontology_by_type(term_type: String): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("mydb", conf)

    val q =
      sql"""
           select distinct ontology
           from svr.apiresults1
           where service ilike 'recommender' and term_type ilike $term_type
         """.as[String]
    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))
    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }

  def get_term_by_ontology (ontology: String, term_type: String): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("mydb", conf)

    val q =
      sql"""
           select distinct parsed_value
           from svr.apiresults1
           where service ilike 'recommender' and term_type ilike $term_type and ontology ilike $ontology
         """.as[String]

    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))
    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }
}






package DBcon

import com.typesafe.config.ConfigFactory

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.PostgresProfile.api._
import java.io._

import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration

object con_handler {

  val parsedConfig = ConfigFactory.parseFile(new File("src/main/scala/DBcon/application.conf"))
  val conf = ConfigFactory.load(parsedConfig)
  val db = Database.forConfig("mydb", conf)

  def run_q1 (search_term: Seq[String]): List[String] = {

    val q1 = sql"""
              select A.value as ID, B.value as NAME
              from svr.T1 as A join svr.T1 as B on A.s_id=B.s_id
              where A.key ilike ${search_term(0)}
              and B.key ilike ${search_term(1)}
              group by A.value,B.value
              order by A.value""".as[(String, String)]

    var resultsBuffer = ListBuffer[String]()
    try {
      val setup = DBIO.seq()
      val setupFuture = db.run(setup)
      val resultFuture = setupFuture.flatMap { _ =>
        db.run(q1).map(_.foreach {a =>
          resultsBuffer.append(a._2)
        })
      }
      Await.result(resultFuture, Duration.Inf)
    }
    finally db.close()
    println("db ok q1\n\n")
    return resultsBuffer.toList
  }

  def run_q2(term: String): List[String] = {
    val db = Database.forConfig("mydb", conf)
    val q2 = sql"""
              select A.value
              from svr.T1 as A join svr.T1 as B on A.s_id=B.s_id
              where A.key ilike 'biosample_term_id'
              and B.key ilike 'Biosample_term_name'
              and B.value ilike $term
              group by A.value,B.value
              order by A.value""".as[(String)]

    var resultsBuffer = ListBuffer[String]()
    try {
      val setup = DBIO.seq()
      val setupFuture = db.run(setup)
      val resultFuture = setupFuture.flatMap { _ =>
        db.run(q2).map(_.foreach {a =>
          resultsBuffer.append(a)
        })
      }
      Await.result(resultFuture, Duration.Inf)
    }
    finally db.close()
    println("db ok q2\n\n")
    return resultsBuffer.toList
  }
}

package DBcon

import scala.concurrent._
import slick.jdbc.PostgresProfile.api._
import java.io._
import java.util.Spliterator.OfPrimitive

import scala.concurrent.ExecutionContext.Implicits.global
import Tables.{cv_support, cv_support_syn, cv_support_xref, onto_support_hyp, user_feedback}

import scala.concurrent.duration.Duration
import com.typesafe.config.ConfigFactory

object gecotest_handler {
  private val parsedConfig = ConfigFactory.parseFile(new File("src/main/scala/DBcon/application.conf"))
  private val conf= ConfigFactory.load(parsedConfig)

  private val cv_support = TableQuery[cv_support]
  private val cv_support_syn = TableQuery[cv_support_syn]
  private val cv_support_xref = TableQuery[cv_support_xref]
  private val onto_support_hyp = TableQuery[onto_support_hyp]
  private val user_feedback = TableQuery[user_feedback]

  val db = Database.forConfig("gecotest1", conf)
  val setup_user_fb = user_feedback.schema.create
  Await.result(db.run(setup_user_fb), Duration.Inf)

  def cv_support_insert(rows: List[List[String]]): Unit = {

    var ok: Seq[(String, String, String)] = Seq()
    for (l <- rows) {
      ok :+= (l(0), l(1), l(2))
    }
    val db = Database.forConfig("gecotest1", conf)
    val insertAction = cv_support ++= ok
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def syn_insert(rows: List[List[String]]): Unit = {
    var ok: Seq[(Int, String, String)] = Seq()

    for (l <- rows) {
      ok :+= (l(0).toInt, l(1), l(2))
    }
    val db = Database.forConfig("gecotest1", conf)
    val insertAction = cv_support_syn ++= ok
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def xref_insert(rows: List[List[String]]): Unit = {
    var ok: Seq[(Int, String, String)] = Seq()

    for (l <- rows) {
      ok :+= (l(0).toInt, l(1), l(2))
    }
    val db = Database.forConfig("gecotest1", conf)
    val insertAction = cv_support_xref ++= ok
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def hyp_insert(rows: List[List[String]]): Unit = {
    var ok: Seq[(Int, Int, String)] = Seq()

    for (l <- rows) {
      ok :+= (l(0).toInt, l(1).toInt, l(2))
    }
    val db = Database.forConfig("gecotest1", conf)
    val insertAction = onto_support_hyp ++= ok
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }


  def get_tid(label: String): Int = {

    val db = Database.forConfig("gecotest1", conf)

    var tid = 0
    val q = cv_support.filter(_.label===label).map(_.tid)

    val resultfuture = db.run(q.result).map(a => tid = a.head)

    Await.result(resultfuture, Duration.Inf)
    db.close()
    tid
  }

  def user_feedback(rows: List[List[String]]): Unit = {
    var ok: Seq[(String, String, String, Option[String], Option[String], Option[String], Option[String])] = Seq()

    for (l <- rows) {
      ok :+= (l(0), l(1), l(2), Option(l(3)), Option(l(4)), Option(l(5)), Option(l(6)))
    }
    val db = Database.forConfig("gecotest1", conf)
    val insertAction = user_feedback.map(a=>(a.column_name,a.table_name,a.raw_value,a.parsed_value,a.label,a.source,a.code))++=ok

    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def get_raw_values(term_type: String): List[String] = {
    val db = Database.forConfig("gecotest1", conf)
    var result: Seq[String]= List()
    val t = term_type

    val m = Map("biosample" -> List("disease","tissue","cell_line"),"donor"->List("ethnicity","species"),"item"->List("platform"),"experiment_type"->List("technique","feature","target"),"container"->List("annotation"))

    val default = (-1,"")
    val table = m.find(_._2.contains(t)).getOrElse(default)._1.toString

    val q =
      sql"""select distinct #$t
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

}

package Enricher.DBCon

import scala.concurrent._
import slick.jdbc.PostgresProfile.api._
import java.sql.{BatchUpdateException, SQLTransientConnectionException}

import Config.config

import scala.concurrent.ExecutionContext.Implicits.global
import Tables.{cv_support, cv_support_raw, cv_support_syn, cv_support_xref, onto_support_hyp, onto_support_hyp_unfolded, ontology, user_changes, user_feedback}

import scala.concurrent.duration.Duration
import slick.jdbc.meta.MTable
import Config.config._
import org.apache.log4j.Logger
import slick.sql.SqlAction

object db_handler {
  val logger: Logger = Logger.getLogger(this.getClass)


  private var _db_name = "gecotest2"
  def db_name: String = _db_name
  def set_db_name(value: String): Unit = _db_name = value

  protected def get_db(): Database = {
    var db: Database = null
    var attempts = 0
    while(db==null & attempts != 5){
      try {
        db = Database.forConfig(_db_name, conf)
      }
      catch {
        case e: TimeoutException => logger.info(e.getCause)
        case e1: SQLTransientConnectionException => logger.info(e1.getCause)
        case e2: Exception => logger.info(e2.getCause)
      }
      attempts += 1
    }
    if(db==null){
      logger.info("Connection to db failed, Exiting")
      sys.exit(-1)
    }
    db
  }

  val tables = List(ontology,cv_support,cv_support_syn,cv_support_xref,cv_support_raw,onto_support_hyp,onto_support_hyp_unfolded,user_changes, user_feedback)

  def init(): Unit = {
    val db = get_db()
    val existing = db.run(MTable.getTables)
    val f = existing.flatMap(v => {
      val names = v.map(mt => mt.name.name)
      val createIfNotExist = tables.filter(table =>
        !names.contains(table.baseTableRow.tableName)
      ).map(_.schema.create)
      db.run(DBIO.sequence(createIfNotExist))
    })
    Await.result(f, Duration.Inf)
    db.close()
  }

  def reset_db(): Unit = {
    val db = get_db()
    val existing = db.run(MTable.getTables)
    val f = existing.flatMap(v => {
      val names = v.map(mt => mt.name.name)
      val dropIfExist = tables.reverse.filter(table =>
        names.contains(table.baseTableRow.tableName)
      ).map(_.schema.drop)
      db.run(DBIO.sequence(dropIfExist))
    })
    Await.result(f, Duration.Inf)
    db.close()
  }

  def null_gcm(): Unit = {
    val tables = config.get_gcm_table_list()
    var setup: DBIOAction[Unit,NoStream,Effect] = DBIO.seq()
    for (table<- tables) {
      val columns = config.get_termtype_list(table)
      for (column <- columns) {
        val column_tid = column + "_tid"
        val q =
          sqlu"""update #$table
                set #$column_tid = NULL"""
        setup = setup.andThen(DBIO.seq(q))
      }
    }
    val db = get_db()
    val f = db.run(setup)
    Await.result(f,Duration.Inf)
  }

  def cv_support_insert(rows: List[cv_support_type]): Unit = {

    var ok: Seq[(String, String, String, String, String)] = Seq()
    for (l <- rows) {
      ok :+= (l.source,l.code,l.label,l.description,l.iri)
    }
    val db = get_db()
    val insertAction = cv_support.map(a=> (a.source,a.code,a.label,a.description,a.iri)) ++= ok
    try {
      val insert = db.run(insertAction)
      Await.result(insert, Duration.Inf)
    }
    catch {
      case e: BatchUpdateException => e.getNextException.printStackTrace()
    }
    db.close()
  }

  def syn_insert(rows: List[cv_support_syn_type]): Unit = {
    var ok: Seq[(Int, String, String)] = Seq()

    val db = get_db()
    val insertAction = cv_support_syn ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def xref_insert(rows: List[cv_support_xref_type]): Unit = {
    var ok: Seq[(Int, String, String)] = Seq()

    val db = get_db()
    val insertAction = cv_support_xref ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }


  def raw_insert(rows: List[cv_support_raw_type]): Unit = {
    var ok: Seq[(Int, String, String, String, Char)] = Seq()

    val db = get_db()
    Await.result(db.run(cv_support_raw ++= rows),Duration.Inf)
    db.close()
  }


  def hyp_insert(rows: List[onto_support_hyp_type]): Unit = {
    val db = get_db()
    val insertAction = onto_support_hyp ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }


  def get_tid(source: String, code: String): Int = {
    val db = get_db()
    var tid = 0
    val q = cv_support.filter(a => a.source === source && a.code === code).map(_.tid)
    val resultfuture = db.run(q.result).map(a => tid = a.head)
    Await.result(resultfuture, Duration.Inf)
    db.close()
    tid
  }

  def get_user_feedback_raw_values(table_name: String, column_name: String): List[String] = {
    val db = get_db()
    var result: List[String] = List()

    val q = user_feedback.filter(t => t.table_name === table_name && t.column_name === column_name && t.resolved === false).map(_.raw_value).result

    val result_future = db.run(q).map(a => result = a.toList.distinct)
    Await.result(result_future, Duration.Inf)
    db.close()

    result
  }

  def get_user_feedback_infos(raw_value: String): List[user_feedback_type] = {
    val db = get_db()
    var result: List[user_feedback_type] = List()

    val q = user_feedback.filter(t => t.raw_value === raw_value && t.code != null).result


    val result_future = db.run(q).map(a => result = a.toList)
    Await.result(result_future, Duration.Inf)
    db.close()

    result
  }

  def user_feedback_insert(rows: List[user_feedback_type]): Unit = {
    var ok: Seq[(String, String, Option[Int], String, Option[String], Option[String], Option[String], Option[String],Option[String],String)] = Seq()

    for (l <- rows) {
      ok :+= (l.table, l.column, l.tid, l.raw_value, l.parsed_value, l.label, l.source, l.code,l.iri,l.provenance)
    }
    val db = get_db()
    val insertAction = user_feedback.map(a => (a.table_name, a.column_name, a.tid, a.raw_value, a.parsed_value, a.label, a.source, a.code,a.iri,a.provenance)) ++= ok

    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def get_raw_values(table: String, term_type: String): List[String] = {
    val db = get_db()
    var result: Seq[String] = List()
    val t = term_type

    val default = (-1, "")
    val type_tid = t + "_tid"
    val q =
    sql"""select distinct #$t as value
           from #$table
           where #$t IS NOT NULL AND
           #$type_tid IS NULL"""
    try {
      val result_future = db.run(q.as[String]).map(_.foreach(a =>
      result :+= a))
      Await.result(result_future, Duration.Inf)
    }
    finally db.close()
    result.toList
  }

  def insert_user_changes(rows: user_changes_type): Unit = {
    val db = get_db()
    val insertAction = user_changes.map(a => (a.table_name, a.column_name, a.raw_value, a.source, a.code)) ++= Seq((rows.table_name,rows.column_name,rows.raw_value,rows.source,rows.code))
    val insert = db.run(insertAction)

    Await.result(insert, Duration.Inf)
    db.close()
  }


  def get_cv_support_syn_by_value(raw_value: String): cv_support_syn_type = {
    var result = cv_support_syn_type()
    val db = get_db()
    val q = cv_support_syn.filter(a => a.label === raw_value).result.headOption
    val f = db.run(q).map(a =>
      if(a.isDefined)
        result = cv_support_syn_type(a.get.tid, a.get.label,a.get.ttype))
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def check_completeness(raw_value: String): Boolean = {
    var result = false

    val q = cv_support_syn.filter(a => a.label === raw_value && a.ttype === "raw").exists

    val db = get_db()

    Await.result(db.run(q.result).map(a => result = a),Duration.Inf)

    result
  }

  def update_tid(value: String, new_tid: Option[Int]): List[(String,String)] = {
    val result = get_value_info(value)
    for((table_name, column_name) <- result) {
      val tid = column_name + "_tid"

      val q =
        if(new_tid.isDefined) {
          sqlu"""
             update #$table_name
             set #$tid = ${new_tid.get}
             where #$column_name = $value
            """
        }
        else {
          sqlu"""
             update #$table_name
             set #$tid = null
            where #$column_name = $value
            """
        }
      val db = get_db()

      val f = db.run(q)
      Await.result(f, Duration.Inf)
      db.close()
    }
    result
  }

  def get_value_info(value: String): List[(String,String)] = {
    var result: List[(String,String)] = List()
    for (table_name <- get_gcm_table_list()){
      for(column_name <- get_termtype_list(table_name)){
        val q =
          sql"""
                 select distinct #$column_name
                 from #$table_name
                 where #$column_name ilike $value
            """.as[String]
        val db = get_db()
        val f = db.run(q).map(a =>
          if(a.nonEmpty){
            a.foreach(b => result:+= (table_name,column_name))
          }
        )
        Await.result(f, Duration.Inf)
        db.close()
      }
    }
    result
  }

  def get_raw_user_changes(table_name: String, column_name: String, raw_value: String): (String, String) = {
    val db = get_db()
    var result = ("null","null")
    val q = user_changes.filter(a=>a.table_name===table_name && a.column_name===column_name && a.raw_value===raw_value).map(a=>(a.source,a.code))
    val f = db.run(q.result).map(a=>
      if(a.nonEmpty)
        result=a.head
    )
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

//  INPUT SOURCE, CODE
//  RETURNS TRUE IF SOURCE, CODE EXIST IN cv_support
  def cv_support_exists(source: String, code: String): Boolean = {
    val db = get_db()
    var result = false
    val q = cv_support.filter(a=> a.source===source && a.code===code).map(a=>(a.source,a.code))
    val f = db.run(q.result).map(a=>
      result = a.nonEmpty
    )

    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def set_resolved(raw_value: String): Unit = {
    val q = user_feedback.filter(_.raw_value===raw_value).map(_.resolved)
    val update = q.update(true)
    val db = get_db()
    val f = db.run(update)
    Await.result(f,Duration.Inf)
    db.close()
  }


  def get_cv_support_by_tid(tid: Int): cv_support_type = {
    val db = get_db()
    var result = cv_support_type()
    val q = cv_support.filter(_.tid===tid).result.headOption
    val f = db.run(q).map(a=>
      if(a.isDefined)
        result = a.get
    )
    Await.result(f,Duration.Inf)
    db.close()
    result
  }

  def get_tid_parent_distinct(tid_parent: Int): List[Int] = {
    var result: List[Int] = List()

    val q = onto_support_hyp.filter(_.tid_p>=tid_parent).map(_.tid_p).distinct.result
    val db = get_db()
    Await.result(db.run(q).map(a=> result = a.toList),Duration.Inf)
    db.close()
    result
  }

  def get_onto_hyp(tid_parent: Int): List[onto_support_hyp_type] = {
    val db = get_db()
    var result: List[onto_support_hyp_type] = List()

    val q = onto_support_hyp.filter(_.tid_p===tid_parent).result

    val f = db.run(q).map(a =>
      result = a.toList
    )
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def insert_unfolded(rows: List[onto_support_hyp_unfolded_type]): Unit = {
    val db = get_db()

    val insertAction = onto_support_hyp_unfolded ++= rows
    try {
      Await.result(db.run(insertAction), Duration.Inf)
    }
    catch {
      case e: BatchUpdateException => e.getNextException.printStackTrace()
    }
    db.close()
  }

  def onto_exist(onto: String): Boolean = {
    val q = ontology.filter(_.source === onto).exists
    val db = get_db()
    var res = false
    val f = db.run(q.result).map(a => res = a)

    Await.result(f, Duration.Inf)
    db.close()
    res
  }

  def insert_ontology (rows: ontology_type): Unit = {
    val db = get_db()

    val insert = ontology ++= List(rows)

    Await.result(db.run(insert),Duration.Inf)
    db.close()
  }

  def user_fb_exist(value: String, source: String, code: String): Boolean = {
    val q = user_feedback.filter(a => a.source===source && a.code === code && a.raw_value===value).exists
    val db = get_db()
    var res = false
    val f = db.run(q.result).map(a => res = a)

    Await.result(f, Duration.Inf)
    db.close()
    res
  }
}

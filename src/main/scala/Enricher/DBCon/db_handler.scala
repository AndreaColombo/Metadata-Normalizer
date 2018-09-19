package Enricher.DBCon

import java.sql.{BatchUpdateException, SQLTransientConnectionException}

import Config.config
import Config.config._
import Enricher.DBCon.Tables._
import org.apache.log4j.Logger
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

object db_handler {
  val logger: Logger = Logger.getLogger(this.getClass)


  private var _db_name = "gecotest2"
  def db_name: String = _db_name
  def set_db_name(value: String): Unit = _db_name = value

  def get_db(): Database = {
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

  val tables = List(ontology,vocabulary,synonym,reference,raw_annotation,relationship,relationship_unfolded,expert_preference,expert_choice)

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

  def cv_support_insert(rows: List[vocabulary_type]): Unit = {

    var ok: Seq[(String, String, String, String, String)] = Seq()
    for (l <- rows) {
      ok :+= (l.source,l.code,l.label,l.description,l.iri)
    }
    val db = get_db()
    val insertAction = vocabulary.map(a=> (a.source,a.code,a.label,a.description,a.iri)) ++= ok
    try {
      val insert = db.run(insertAction)
      Await.result(insert, Duration.Inf)
    }
    catch {
      case e: BatchUpdateException => e.getNextException.printStackTrace()
    }
    db.close()
  }

  def syn_insert(rows: List[synonym_type]): Unit = {
    var ok: Seq[(Int, String, String)] = Seq()

    val db = get_db()
    val insertAction = synonym ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def xref_insert(rows: List[reference_type]): Unit = {
    var ok: Seq[(Int, String, String)] = Seq()

    val db = get_db()
    val insertAction = reference ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }


  def raw_insert(rows: List[raw_annotation_type]): Unit = {
    var ok: Seq[(Int, String, String, String, Char)] = Seq()

    val db = get_db()
    Await.result(db.run(raw_annotation ++= rows),Duration.Inf)
    db.close()
  }


  def hyp_insert(rows: List[relationship_type]): Unit = {
    val db = get_db()
    val insertAction = relationship ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }


  def get_tid(source: String, code: String): Int = {
    val db = get_db()
    var tid = 0
    val q = vocabulary.filter(a => a.source === source && a.code === code).map(_.tid)
    val resultfuture = db.run(q.result).map(a => tid = a.head)
    Await.result(resultfuture, Duration.Inf)
    db.close()
    tid
  }

  def get_user_feedback_raw_values(table_name: String, column_name: String): List[String] = {
    val db = get_db()
    var result: List[String] = List()

    val q = expert_choice.filter(t => t.table_name === table_name && t.column_name === column_name && t.resolved === false).map(_.raw_value).result

    val result_future = db.run(q).map(a => result = a.toList.distinct)
    Await.result(result_future, Duration.Inf)
    db.close()

    result
  }

  def get_user_feedback_infos(raw_value: String): List[expert_choice_type] = {
    val db = get_db()
    var result: List[expert_choice_type] = List()

    val q = expert_choice.filter(t => t.raw_value === raw_value && t.code != null).result


    val result_future = db.run(q).map(a => result = a.toList)
    Await.result(result_future, Duration.Inf)
    db.close()

    result
  }

  def user_feedback_insert(rows: List[expert_choice_type]): Unit = {
    var ok: Seq[(String, String, Option[Int], String, Option[String], Option[String], Option[String], Option[String],Option[String],String,String)] = Seq()

    for (l <- rows) {
      ok :+= (l.table, l.column, l.tid, l.raw_value, l.parsed_value, l.label, l.source, l.code,l.iri,l.provenance,l.timestamp)
    }
    val db = get_db()
    val insertAction = expert_choice.map(a => (a.table_name, a.column_name, a.tid, a.raw_value, a.parsed_value, a.label, a.source, a.code,a.iri,a.provenance,a.timestamp)) ++= ok

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
    sql"""select distinct lower (#$t)
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

  def insert_user_changes(rows: expert_preference_type): Unit = {
    val db = get_db()
    val insertAction = expert_preference.map(a => (a.table_name, a.column_name, a.raw_value, a.source, a.code)) ++= Seq((rows.table_name,rows.column_name,rows.raw_value,rows.source,rows.code))
    val insert = db.run(insertAction)

    Await.result(insert, Duration.Inf)
    db.close()
  }


  def get_cv_support_syn_by_value(raw_value: String): synonym_type = {
    var result = synonym_type()
    val db = get_db()
    val q = synonym.filter(a => a.label.toLowerCase === raw_value.toLowerCase).result.headOption
    val f = db.run(q).map(a =>
      if(a.isDefined)
        result = a.get)
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def get_cv_support_raw(where: raw_annotation => Rep[Boolean]): raw_annotation_type = {
    var result = raw_annotation_type()
    val db = get_db()
    raw_annotation.filter(a => a.table_name === "biosample")
    val q = for {
      a: raw_annotation <- raw_annotation
//      where(a)
       if where(a)
    } yield a


    val f = db.run(q.result.headOption).map(a =>
      if(a.isDefined)
        result = a.get
    )
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def update_tid(value: String, new_tid: Option[Int],table_name: String, column_name: String): Unit = {
    val tid = column_name + "_tid"
    val q =
      if(new_tid.isDefined) {
        sqlu"""
           update #$table_name
           set #$tid = ${new_tid.get}
           where #$column_name ilike $value
          """
      }
      else {
        sqlu"""
           update #$table_name
           set #$tid = null
           where #$column_name ilike $value
          """
      }
    val db = get_db()
    val f = db.run(q)
    Await.result(f, Duration.Inf)
    db.close()
  }

  def get_value_info(value: String, table_name: String, column_name: String): List[(String,String)] = {
    var result: List[(String,String)] = List()
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
    result
  }

  def get_raw_user_changes(table_name: String, column_name: String, raw_value: String): (String, String) = {
    val db = get_db()
    var result = ("null","null")
    val q = expert_preference.filter(a=>a.table_name===table_name && a.column_name===column_name && a.raw_value.toLowerCase===raw_value.toLowerCase).map(a=>(a.source,a.code))
    val f = db.run(q.result).map(a=>
      if(a.nonEmpty)
        result=a.head
    )
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

//  INPUT SOURCE, CODE
//  RETURNS TRUE IF SOURCE, CODE EXIST IN vocabulary
  def cv_support_exists(source: String, code: String): Boolean = {
    val db = get_db()
    var result = false
    val q = vocabulary.filter(a=> a.source===source && a.code===code).map(a=>(a.source,a.code))
    val f = db.run(q.result).map(a=>
      result = a.nonEmpty
    )

    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def set_resolved(raw_value: String, table_name: String, column_name: String): Unit = {
    val q = expert_choice.filter(a => a.raw_value===raw_value && a.table_name===table_name && a.column_name === column_name).map(_.resolved)
    val update = q.update(true)
    val db = get_db()
    val f = db.run(update)
    Await.result(f,Duration.Inf)
    db.close()
  }


  def get_cv_support_by_tid(tid: Int): vocabulary_type = {
    val db = get_db()
    var result = vocabulary_type()
    val q = vocabulary.filter(_.tid===tid).result.headOption
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

    val q = relationship.filter(_.tid_p>=tid_parent).map(_.tid_p).distinct.result
    val db = get_db()
    Await.result(db.run(q).map(a=> result = a.toList),Duration.Inf)
    db.close()
    result
  }

  def get_onto_hyp(tid_parent: Int): List[relationship_type] = {
    val db = get_db()
    var result: List[relationship_type] = List()

    val q = relationship.filter(_.tid_p===tid_parent).result

    val f = db.run(q).map(a =>
      result = a.toList
    )
    Await.result(f, Duration.Inf)
    db.close()
    result
  }

  def insert_unfolded(rows: List[relationship_unfolded_type]): Unit = {
    val db = get_db()

    val insertAction = relationship_unfolded ++= rows
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
    val q = expert_choice.filter(a => a.source===source && a.code === code && a.raw_value===value).exists
    val db = get_db()
    var res = false
    val f = db.run(q.result).map(a => res = a)

    Await.result(f, Duration.Inf)
    db.close()
    res
  }

  def clean_user_feedback(table: String, column: String): Unit = {
    val q = expert_choice.filter(a => a.table_name === table && a.column_name === column && a.resolved === false).delete
    val db = get_db()
    Await.result(db.run(q),Duration.Inf)
    db.close()
  }

  def get_suggestions_raw(raw_value: String, table_name: String, column_name:String): List[String] = {
    var suggestion: List[String] = List()
    val value_clean = "%"+raw_value.replaceAll("[ ,!.\\-/]+","%")+"%"
    val q =
      sql"""
         select distinct #$column_name
         from #$table_name
         where #$column_name ilike $value_clean
         """.as[String]

    val db = get_db()
    val f = db.run(q).map(_.foreach(a =>
    suggestion :+= a
    ))
    Await.result(f, Duration.Inf)
    db.close()
    suggestion
  }
}

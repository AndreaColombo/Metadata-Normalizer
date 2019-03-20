package enricher.dbcon

import java.sql.{BatchUpdateException, SQLTransientConnectionException}

import config_pkg.ApplicationConfig._
import enricher.dbcon.Tables._
import enricher.engine.RawValue
import org.apache.log4j.Logger
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.meta.MTable

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

/**
  * This object contains all the methods the Enricher Engine uses to interface with the database
  */
//noinspection SqlNoDataSourceInspection
object DbHandler {
  val logger: Logger = Logger.getLogger(this.getClass)

  /**
    * Current database
    */
  private var _db_name = "gmql_meta_new2"

  /**
    * Getter method for database name
    *
    * @return
    */
  def db_name: String = _db_name

  /**
    * Setter method for database name
    *
    * @param value new db name
    */
  def set_db_name(value: String): Unit = _db_name = value

  /**
    * Method that handles the connection to the database and creates a databse object
    *
    * @return A database object
    */
  var db: Database = Database.forConfig(_db_name, conf)

  def close_db() = db.close()

  def get_db(): Database = {
    //    do select 1 if error then db = null

    val q =
      sql"""
           select 1""".as[String]
    try {
      db.run(q)
    }
    catch {
      case e: Exception => db = null
    }
    var attempts = 0
    while (db == null & attempts != 5) {
      try {
        db = Database.forConfig(_db_name, conf)
        Thread.sleep(5000)
      }
      catch {
        case e: TimeoutException => logger.info(e.getCause)
        case e1: SQLTransientConnectionException => logger.info(e1.getCause)
        case e2: Exception => logger.info(e2.getCause)
      }
      attempts += 1
      logger.info("Connecting to db attempt " + attempts)
    }
    if (db == null) {
      logger.info("Connection to db failed, Exiting")
      sys.exit(-1)
    }
    logger.info("DB ok")
    db
  }

  val tables = List(ontology, vocabulary, synonym, reference, raw_annotation, relationship, expert_preference, expert_choice, expert_feedback, relationship_unfolded)

  /**
    * Drops foreign key references on the GCM tables
    */
  def drop_fk_gcm(): Unit = {
    logger.info("Dropping gcm fk")
    val tables = get_gcm_table_list()
    var setup: DBIOAction[Unit, NoStream, Effect] = DBIO.seq()
    for (table <- tables) {
      val columns = get_termtype_list(table)
      for (column <- columns) {
        val column_tid = column + "_tid"
        val constraint_name = table + "_" + column_tid + "_fk"
        val q2 =
          sqlu"""ALTER TABLE #$table
        DROP CONSTRAINT #$constraint_name;
        """
        setup = setup.andThen(DBIO.seq(q2))
      }
    }
    val db = get_db()
    val f = db.run(setup)
    Await.result(f, Duration.Inf)
    // db.close()
  }

  /**
    * Creates the local knowledge base tables
    */
  def init(): Unit = {
    logger.info("Init")
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
    // db.close()
  }

  /**
    * Drop all local KB tables
    */
  def reset_db(): Unit = {
    logger.info("Resetting db")
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
    // db.close()
  }

  /**
    * Creates the foreign key reference on the gcm tables
    */
  def create_fk_gcm(): Unit = {
    logger.info("Creating gcm fk")
    val tables = get_gcm_table_list()
    var setup: DBIOAction[Unit, NoStream, Effect] = DBIO.seq()
    for (table <- tables) {
      val columns = get_termtype_list(table)
      for (column <- columns) {
        val column_tid = column + "_tid"
        val constraint_name = table + "_" + column_tid + "_fk"
        val q2 =
          sqlu"""alter table #$table ADD CONSTRAINT #$constraint_name FOREIGN KEY (#$column_tid) REFERENCES vocabulary (tid) on DELETE SET NULL"""
        setup = setup.andThen(DBIO.seq(q2))
      }
    }
    val db = get_db()
    val f = db.run(setup)
    Await.result(f, Duration.Inf)
    // db.close()
  }

  /**
    * Set all tids of GCM tables to null
    */
  def null_gcm(): Unit = {
    logger.info("resetting gcm")
    val tables = get_gcm_table_list()
    var setup: DBIOAction[Unit, NoStream, Effect] = DBIO.seq()
    for (table <- tables) {
      val columns = get_termtype_list(table)
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
    Await.result(f, Duration.Inf)
    // db.close()
  }

  /**
    * Insert an element of vocabulary in the corresponding table and returns assigned tid
    *
    * @param rows The element to be inserted
    * @return Assigned tid
    */
  def vocabulary_insert(rows: vocabulary_type): Int = {

    val db = get_db()
    var new_tid = -1
    val insertAction = (vocabulary returning vocabulary.map(_.tid) into ((vocabulary, tid) => vocabulary.copy(tid = tid))) += rows
    try {
      val insert = db.run(insertAction).map(a => new_tid = a.tid)
      Await.result(insert, Duration.Inf)
    }
    catch {
      case e: BatchUpdateException => logger.info(e.getNextException)
    }
    // db.close()
    new_tid
  }

  /**
    * Insert a number of elements of synonym in the corresponding table
    *
    * @param rows The elements to be inserted
    */
  def synonym_insert(rows: List[synonym_type]): Unit = {
    val db = get_db()
    val insertAction = synonym ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    // db.close()
  }

  /**
    * Insert a number of elements of reference in the corresponding table
    *
    * @param rows The elements to be inserted
    */
  def reference_insert(rows: List[reference_type]): Unit = {
    val db = get_db()
    val insertAction = reference ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    // db.close()
  }

  /**
    * Insert an element of raw annotation in the corresponding table
    *
    * @param rows The element to be inserted
    */
  def raw_insert(rows: raw_annotation_type): Unit = {
    val db = get_db()
    Await.result(db.run(raw_annotation += rows), Duration.Inf)
    // db.close()
  }

  /**
    * Check existence of a specific relation in the corresponding table, used to prevent duplicates
    *
    * @param row The element to check the existence
    * @return True if relation exists, false otherwise
    */
  def hyp_exist(row: relationship_type): Boolean = {
    val db = get_db()
    val q = relationship.filter(a => a.tid_c === row.tid_c && a.tid_p === row.tid_p && a.rel_type === row.rel_type)

    var exist = false
    val f = db.run(q.exists.result).map(a => exist = a)
    Await.result(f, Duration.Inf)

    // db.close()
    exist
  }

  /**
    * Insert an element of relation in the corresponding table
    *
    * @param rows The element to be inserted
    */
  def hyp_insert(rows: relationship_type): Unit = {
    val db = get_db()
    if (!hyp_exist(rows)) {
      val insertAction = relationship += rows
      val insert = db.run(insertAction)
      Await.result(insert, Duration.Inf)
    }
    // db.close()
  }

  /**
    * Retrieve a tid if the element exists in vocabulary
    *
    * @param source Source of the element
    * @param code   Code of the element
    * @return Tid of the tuple source, code in vocabulary if exists, none otherwise
    */
  def get_tid_option(source: String, code: String): Option[Int] = {
    logger.info("ontology: " + source)
    logger.info("code: " + code)
    val db = get_db()
    var tid: Option[Int] = None
    val q = vocabulary.filter(a => a.source === source && a.code === code).map(_.tid)
    val resultfuture = db.run(q.result).map(a => tid = a.headOption)
    Await.result(resultfuture, Duration.Inf)
    // db.close()
    tid
  }

  /**
    * Retrieve all raw values of a specific type in the expert choice table
    *
    * @param table_name  Provenance table of the raw values to retrieve
    * @param column_name Provenance column of the raw values
    * @return A list of distinct raw values
    */
  def get_user_feedback_raw_values(table_name: String, column_name: String): List[String] = {
    val db = get_db()
    var result: List[String] = List()

    val q = expert_choice.filter(t => t.table_name === table_name && t.column_name === column_name && t.resolved === false).map(_.raw_value).result

    val result_future = db.run(q).map(a => result = a.toList.distinct)
    Await.result(result_future, Duration.Inf)
    // db.close()

    result
  }

  /**
    * Retrieve all rows corresponding to a raw value in the expert choice table
    *
    * @param raw_value the raw value
    * @return A list of elements of expert choice
    */
  def get_user_feedback_infos(raw_value: String): List[expert_choice_type] = {
    val db = get_db()
    var result: List[expert_choice_type] = List()

    val q = expert_choice.filter(t => t.raw_value === raw_value && t.code != null).result


    val result_future = db.run(q).map(a => result = a.toList)
    Await.result(result_future, Duration.Inf)
    // db.close()

    result
  }

  /**
    * Insert a list of elements of expert choice in the corresponding table
    *
    * @param rows The element to be inserted
    */
  def user_feedback_insert(rows: List[expert_choice_type]): Unit = {
    val db = get_db()
    val insertAction = expert_choice ++= rows

    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    // db.close()
  }

  /**
    * Retrieve all raw values from a specific table of GCM that aren't annotated yet, that is: their tid is null
    *
    * @param table     The table from where to retrieve the values
    * @param term_type The column
    * @return a list of raw values
    */
  def get_raw_values(table: String, term_type: String): List[String] = {
    val db = get_db()
    var result: Seq[String] = List()
    val t = term_type

    val type_tid = t + "_tid"
    val q =
      sql"""select distinct lower (#$t)
           from #$table
           where #$t IS NOT NULL AND
           #$type_tid IS NULL"""
    val result_future = db.run(q.as[String]).map(_.foreach(a =>
      result :+= a))
    Await.result(result_future, Duration.Inf)
    //   finally  db.close()
    result.toList
  }

  /**
    * Insert an element of expert preference in the corresponding table
    *
    * @param rows The element to be inserted
    */
  def insert_expert_preference(rows: expert_preference_type): Unit = {
    val db = get_db()
    val insertAction = expert_preference.insertOrUpdate(rows)
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    // db.close()
  }

  /**
    * Retrieve an element of synonym type corresponding to a raw value, if exists
    *
    * @param raw_value The value to check
    * @return An element of synonym type if exists, otherwise an empty element
    */
  def get_synonym_by_value(raw_value: String): synonym_type = {
    var result = synonym_type()
    val db = get_db()
    val q = synonym.filter(a => a.label.toLowerCase === raw_value.toLowerCase).result.headOption
    val f = db.run(q).map(a =>
      if (a.isDefined)
        result = a.get)
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  /**
    * Get an element of raw annotation table based on a user defined query
    *
    * @param where User defined condition
    * @return Element of raw annotation
    */
  def get_raw_annotation(where: raw_annotation => Rep[Boolean]): raw_annotation_type = {
    var result = raw_annotation_type()
    val db = get_db()
    val q = for {
      a: raw_annotation <- raw_annotation
      if where(a)
    } yield a

    val f = db.run(q.result.headOption).map(a =>
      if (a.isDefined)
        result = a.get
    )
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  /**
    * Get an element of ontology based on a user defined query
    *
    * @param where User defined condition
    * @return
    */
  def get_ontology(where: ontology => Rep[Boolean]): Option[ontology_type] = {
    var result = Option(ontology_type())
    val db = get_db()
    val q = for {
      a: ontology <- ontology
      if where(a)
    } yield a


    val f = db.run(q.result.headOption).map(a =>
      result = a
    )
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  def get_ontologies(): List[ontology_type] = {
    val db = get_db()

    var result: List[ontology_type] = List()
    val f = db.run(ontology.result).map(a => result = a.toList)
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  /**
    * Update tid of a raw value in the gcm, new tid could be either a valid number or NULL
    *
    * @param rawValue raw value which tid needs to be updated
    * @param new_tid  New value of the tid
    */
  def update_gcm_tid(rawValue: RawValue, new_tid: Option[Int]): Unit = {
    val table_name = rawValue.table
    val column_name = rawValue.column
    val value = rawValue.value
    val tid = column_name + "_tid"
    val q =
      if (new_tid.isDefined) {
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
    // db.close()
  }

  /**
    * Check the existence of a value of a specific table and columns in the gcm
    *
    * @param value       Value to be checked
    * @param table_name  Provenance table
    * @param column_name Provenance column
    * @return True if value exists, false otherwise
    */
  def value_exist(value: String, table_name: String, column_name: String): Boolean = {
    var result = false
    val q =
      sql"""
             select distinct #$column_name
             from #$table_name
             where #$column_name ilike $value
        """.as[String]
    val db = get_db()
    val f = db.run(q).map(a =>
      result = a.nonEmpty
    )
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  /**
    * Retrieve source and code from expert preference for a specific value of a specific table and column
    *
    * @param table_name  Provenance table
    * @param column_name Provenance column
    * @param raw_value   Value to retrieve information
    * @return A tuple (source,code) if exists
    */
  def get_raw_expert_preference(table_name: String, column_name: String, raw_value: String): (String, String) = {
    val db = get_db()
    var result = ("null", "null")
    val q = expert_preference.filter(a => a.table_name === table_name && a.column_name === column_name && a.raw_value.toLowerCase === raw_value.toLowerCase).map(a => (a.source, a.code))
    val f = db.run(q.result).map(a =>
      if (a.nonEmpty)
        result = a.head
    )
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  /**
    * Mark a raw value in expert choice table as resolved, that is: the expert has taken care of it
    *
    * @param raw_value   The value to update
    * @param table_name  Provenance table
    * @param column_name Provenance column
    */
  def set_resolved(raw_value: String, table_name: String, column_name: String): Unit = {
    val q = expert_choice.filter(a => a.raw_value === raw_value && a.table_name === table_name && a.column_name === column_name).map(_.resolved)
    val update = q.update(true)
    val db = get_db()
    val f = db.run(update)
    Await.result(f, Duration.Inf)
    // db.close()
  }

  /**
    * Retrieve an element of vocabulary by tid
    *
    * @param tid The tid of the element
    * @return An element of vocabulary
    */
  def get_vocabulary_by_tid(tid: Int): vocabulary_type = {
    val db = get_db()
    var result = vocabulary_type()
    val q = vocabulary.filter(_.tid === tid).result.headOption
    val f = db.run(q).map(a =>
      if (a.isDefined)
        result = a.get
    )
    Await.result(f, Duration.Inf)
    // db.close()
    result
  }

  /**
    * Check existence of an ontology in the ontology table
    *
    * @param onto The onto to be checked
    * @return True if onto exists, false otherwise
    */
  def onto_exist(onto: String): Boolean = {
    val q = ontology.filter(_.source === onto).exists
    val db = get_db()
    var res = false
    val f = db.run(q.result).map(a => res = a)

    Await.result(f, Duration.Inf)
    // db.close()
    res
  }

  /**
    * Insert an element of ontology in the corresponding table
    *
    * @param rows The element to be inserted
    */
  def insert_ontology(rows: ontology_type): Unit = {
    val db = get_db()
    val condition = (a: ontology) => a.source === rows.source

    val insert = ontology.filter(condition).result.headOption.flatMap {
      case Some(ontology) =>
        logger.info("Ontology " + rows.source + " already present")
        DBIO.successful(ontology)
      case None =>
        logger.info("Inserting ontology " + rows.source)
        ontology += rows
    }.transactionally

    Await.result(db.run(insert), Duration.Inf)

    // db.close()
  }

  /**
    * Check existence of a tuple (source,code,value) in expert choice table
    *
    * @param value  Value to be checked
    * @param source Source to be checked
    * @param code   Code to be checked
    * @return True if exists
    */
  def expert_choice_exist(value: String, source: String, code: String): Boolean = {
    val q = expert_choice.filter(a => a.source === source && a.code === code && a.raw_value === value).exists
    val db = get_db()
    var res = false
    val f = db.run(q.result).map(a => res = a)

    Await.result(f, Duration.Inf)
    // db.close()
    res
  }

  /**
    * Deletes all unresolved rows with specific table and column
    *
    * @param table  GCM Provenance table
    * @param column GCM Provenance column
    */
  def clean_user_feedback(table: String, column: String): Unit = {
    val q = expert_choice.filter(a => a.table_name === table && a.column_name === column && a.resolved === false).delete
    val db = get_db()
    Await.result(db.run(q), Duration.Inf)
    // db.close()
  }

  /**
    * Retrieve from a specific gcm table and column all raw values similar to a given raw value
    *
    * @param raw_value   The given value
    * @param table_name  Provenance table
    * @param column_name Provenance column
    * @return A list of raw values similar to the given one
    */
  def get_suggestions_raw(raw_value: String, table_name: String, column_name: String): List[String] = {
    var suggestion: List[String] = List()
    val value_clean = "%" + raw_value.replaceAll("[ ,!.\\-/]+", "%") + "%"
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
    // db.close()
    suggestion
  }

  /**
    * Retrieve infos for expert feedback routine
    *
    * @param table_name  GCM Provenance table
    * @param column_name GCM Provenance column
    * @param username    Username
    * @return
    */
  def get_info_for_feedback(table_name: String, column_name: String, username: String): List[expert_info_for_feedback] = {
    var info: List[expert_info_for_feedback] = List()
    val q =
      sql"""
           select vocabulary.tid, label as raw_value, pref_label, source, code, iri, description
           from vocabulary join raw_annotation on vocabulary.tid = raw_annotation.tid
           where table_name ilike $table_name and column_name ilike $column_name and
           iri not in (select raw_value from expert_feedback where expert_username = $username)
         """.as[(Int, String, String, String, String, String, String)]

    val db = get_db()

    val f = db.run(q).map(_.foreach(a =>
      info :+= expert_info_for_feedback(a._1, a._2, a._3, a._4, a._5, a._6, a._7)
    ))
    Await.result(f, Duration.Inf)
    // db.close()

    info
  }

  /**
    * Get the list of username in expert feedback table
    *
    * @return
    */
  def get_username_list(): List[String] = {
    var res: List[String] = List()
    val q = expert_feedback.map(_.expert_username).distinct

    val db = get_db()
    val f = db.run(q.result).map(a => res = a.toList)

    Await.result(f, Duration.Inf)
    // db.close()
    res
  }

  /**
    * Insert a list of elements of expert feedback in the corresponding table
    *
    * @param rows The row to be inserted
    */
  def insert_expert_feedback(rows: List[expert_feedback_type]): Unit = {
    val insertAction = expert_feedback ++= rows
    val db = get_db()
    val f = db.run(insertAction)
    Await.result(f, Duration.Inf)
    // db.close()
  }

  def delete_ontology(onto: String): Unit = {
    val db = get_db()
    val q = ontology.filter(_.source === onto).delete
    val f = db.run(q)
    Await.result(f, Duration.Inf)
    // db.close()
  }

  def unfold(): Unit = {
    val db = get_db()
    val query_tmp =
      sqlu"""create table unfold_tmp (
            tid_anc int,
            tid_desc int,
            depth int,
            rel_type varchar(8)
            )"""
    Await.result(db.run(query_tmp), Duration.Inf)
    logger.info("Temporary table created")

    val rec_query = sqlu""" insert into unfold_tmp (with recursive rel_unfolded(tid_anc, tid_desc, depth, path, rel_type) as (
             select r.tid_parent, r.tid_child, 1, array [row (r.tid_parent, r.tid_child, r.rel_type)], r.rel_type
             from relationship r
             union all
             select ru.tid_anc,
                    ru.tid_desc,
                    ru.depth + 1,
                    path || row (r.tid_parent, r.tid_child,r.rel_type),
                    case ru.rel_type
                      when r.rel_type then ru.rel_type
                      else 'MIXED'
                      end::varchar(8)
                    from relationship r,
                  rel_unfolded ru
             where ru.tid_desc = r.tid_parent)
             select tid_anc, tid_desc, depth, rel_type
             from rel_unfolded)"""

    val insert_query =
      sqlu"""
         insert into relationship_unfolded (
         select tid_anc, tid_desc, min(depth) as distance, rel_type
         from unfold_view
         group by tid_anc, tid_desc, rel_type
         union
         select tid, tid, 0, 'self'
         from vocabulary
      );
        drop table unfold_tmp"""

    val f1 = db.run(rec_query)
    Await.result(f1, Duration.Inf)
    logger.info("Recursive query executed, temp table filled")
    val f2 = db.run(insert_query)
    Await.result(f2, Duration.Inf)
    logger.info("Relationship unfolded filled")
    logger.info("Unfolding complete")
  }
}

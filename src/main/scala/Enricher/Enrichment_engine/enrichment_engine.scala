package Enricher.Enrichment_engine


import Config.config
import Enricher.DBCon.Tables._
import Enricher.DBCon.{cv_support_raw_type, db_handler, default_values, user_feedback_type}
import Utilities.Utils.get_timestamp
import org.slf4j.LoggerFactory
import slick.jdbc.PostgresProfile.api._


object enrichment_engine {

  def controller(column_name: String): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val table_name = config.get_table_by_column(column_name)
    db_handler.clean_user_feedback(table_name,column_name)
    val raw_values = db_handler.get_raw_values(table_name,column_name)
    logger.info(column_name)
    for (raw_value <- raw_values) {
      val condition = (a: cv_support_raw) => a.table_name === table_name && a.column_name === column_name && a.label.toLowerCase === raw_value.toLowerCase
      val result_raw = db_handler.get_cv_support_raw(condition)

      val result_syn = db_handler.get_cv_support_syn_by_value(raw_value.map(_.toLower))
      val result_user_changes = db_handler.get_raw_user_changes(table_name, column_name, raw_value.map(_.toLower))
      //LOCAL KB LOOKUP
      if (result_raw.tid != default_values.int) {
        //VALUE FOUND RAW
        logger.info(s"Value \"$raw_value\" found as RAW in local KB")
        db_handler.update_tid(raw_value, Some(result_syn.tid))
      }
      else if (result_user_changes._1 != "null") {
        //VALUE FOUND IN USER CHANGES
        logger.info(s"Value \"$raw_value\" found in user changes")
        val source = result_user_changes._1
        val a = Ols_interface.ols_get_onto_info(source)
        if (!db_handler.onto_exist(a.source)) {
          db_handler.insert_ontology(a)
        }
        db_interface.db_interface(annotator.get_info(result_user_changes._1, result_user_changes._2, raw_value, table_name, column_name), raw_value, table_name, column_name, 'U')
      }
      else if(result_syn.tid != default_values.int) {
        //VALUE FOUND PREF OR SYN
        if (result_syn.ttype == "pref") {
          logger.info(s"Value \"$raw_value\" found as PREF in local KB")
          val suggestion = db_handler.get_cv_support_by_tid(result_syn.tid)
          if (!db_handler.user_fb_exist(raw_value, suggestion.source, suggestion.code)) {
            db_handler.user_feedback_insert(List(user_feedback_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:PREF", get_timestamp())))
          }
        }
        else {
          logger.info(s"Value \"$raw_value\" found as SYN in local KB")
          val suggestion = db_handler.get_cv_support_by_tid(result_syn.tid)
          if (!db_handler.user_fb_exist(raw_value, suggestion.source, suggestion.code)) {
            db_handler.user_feedback_insert(List(user_feedback_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:SYN", get_timestamp())))
          }
        }
      }

      //ONLINE KB LOOKUP
      else {
        val result_search = annotator.search_term(raw_value, column_name)
        //BEST MATCH FOUND
        if (result_search.options.nonEmpty) {
          if (result_search.options.length == 1) {
            logger.info(s"Value \"$raw_value\" best match found in online KB")
            val source = result_search.options.head.source
            val code = result_search.options.head.code
            val a = Ols_interface.ols_get_onto_info(source)
            if (!db_handler.onto_exist(a.source)) {
              db_handler.insert_ontology(a)
            }
            if (db_handler.cv_support_exists(source, code)) {
              val tid = db_handler.get_tid(source, code)
              val condition = (a: cv_support_raw) => a.tid === tid
              val existing_value = db_handler.get_cv_support_raw(condition)
              if (existing_value.table_name != table_name ||
                existing_value.column_name != column_name ||
                existing_value.label != raw_value) {
                db_handler.raw_insert(List(cv_support_raw_type(tid, raw_value, table_name, column_name, 'R')))
              }
            }
            val result = annotator.get_info(source,code, raw_value, table_name, column_name)
            db_interface.db_interface(result, raw_value, table_name, column_name, 'O')
          }
          else {
            //MULTIPLE RESULTS, BEST MATCH UNDECIDED
            logger.info(s"Best match undecided for value \"$raw_value\"")
            for (elem <- result_search.options){
              val source = elem.source
              val code = elem.code
              val label = elem.label
              db_handler.user_feedback_insert(List(user_feedback_type(default_values.int, default_values.bool, table_name, column_name, null, raw_value, null, Some(label), Some(source), Some(code), Some(Ols_interface.ols_get_iri(source,code)), "LOCAL:UNDECIDED "+result_search.score, get_timestamp())))
            }
          }
        }
        else {
          annotator.get_user_feedback(raw_value,table_name,column_name)
        }
      }
    }
  }
}
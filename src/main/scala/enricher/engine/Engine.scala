package enricher.engine


import config_pkg.ApplicationConfig
import enricher.dbcon.Tables._
import enricher.dbcon._
import utilities.Utils.get_timestamp
import org.slf4j.LoggerFactory
import slick.jdbc.PostgresProfile.api._


object Engine {

  def controller(column_name: String): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val table_name = ApplicationConfig.get_table_by_column(column_name)
    DbHandler.clean_user_feedback(table_name,column_name)
    val raw_values = DbHandler.get_raw_values(table_name,column_name)
    logger.info(column_name)
    for (raw_value <- raw_values) {
      val condition = (a: raw_annotation) => a.table_name === table_name && a.column_name === column_name && a.label.toLowerCase === raw_value.toLowerCase
      val result_raw = DbHandler.get_cv_support_raw(condition)

      val result_syn = DbHandler.get_cv_support_syn_by_value(raw_value.map(_.toLower))
      val result_user_changes = DbHandler.get_raw_user_changes(table_name, column_name, raw_value.map(_.toLower))
      //LOCAL KB LOOKUP
      if (result_raw.tid != default_values.int) {
        //VALUE FOUND RAW
        logger.info(s"""Value "$raw_value" found as RAW in local KB""")
        DbHandler.update_tid(raw_value, Some(result_syn.tid),table_name,column_name)
      }
      else if (result_user_changes._1 != "null") {
        //VALUE FOUND IN USER CHANGES
        logger.info(s"""Value "$raw_value" found in user changes""")
        val source = result_user_changes._1
        val code = result_user_changes._2
        val a = Ols_interface.ols_get_onto_info(source)
        if (!DbHandler.onto_exist(a.source)) {
          DbHandler.insert_ontology(a)
        }
        //TODO ARIF check source code
//        DbInterface.db_interface(Annotator.get_info(result_user_changes._1, result_user_changes._2, raw_value, table_name, column_name), raw_value, table_name, column_name, 'U', source, code)
      }
      else if(result_syn.tid != default_values.int) {
        //VALUE FOUND PREF OR SYN
        if (result_syn.ttype == "pref") {
          logger.info(s"""Value "$raw_value" found as PREF in local KB""")
          val suggestion = DbHandler.get_cv_support_by_tid(result_syn.tid)
          if (!DbHandler.user_fb_exist(raw_value, suggestion.source, suggestion.code)) {
            DbHandler.user_feedback_insert(List(expert_choice_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:PREF", get_timestamp())))
          }
        }
        else {
          logger.info(s"""Value "$raw_value" found as SYN in local KB""")
          val suggestion = DbHandler.get_cv_support_by_tid(result_syn.tid)
          if (!DbHandler.user_fb_exist(raw_value, suggestion.source, suggestion.code)) {
            DbHandler.user_feedback_insert(List(expert_choice_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:SYN", get_timestamp())))
          }
        }
      }

      //ONLINE KB LOOKUP
//      else {
//        val result_search = Annotator.search_term(raw_value, column_name)
        //BEST MATCH FOUND
//        if (result_search.options.nonEmpty) {
//          if (result_search.options.length == 1) {
//            logger.info(s"""Value "$raw_value" best match found in online KB""")
//            val source = result_search.options.head.source
//            val code = result_search.options.head.code
//            val a = Ols_interface.ols_get_onto_info(source)
//            if (!DbHandler.onto_exist(a.source)) {
//              DbHandler.insert_ontology(a)
//            }
//            val result = Annotator.get_info(source,code, raw_value, table_name, column_name)
//            if (DbHandler.cv_support_exists(source, code) && result.isEmpty) {
//              val tid = DbHandler.get_tid(source, code)
//              val condition = (a: raw_annotation) => a.tid === tid
//              val existing_value = DbHandler.get_cv_support_raw(condition)
//              NOTE: ARIF no need to do here, it will do in  DbInterface.DbInterface( line around 89
//              if (existing_value.table_name != table_name ||
//              existing_value.column_name != column_name ||
//              existing_value.iri != raw_value) {
//                DbHandler.raw_insert(List(raw_annotation_type(tid, raw_value, table_name, column_name, 'O')))
//                DbHandler.synonym_insert(List(synonym_type(tid,raw_value,"raw")))
//              }
//            }
            //TODO ARIF ILLUMINA adds the tid of the first parent that insert into the db
            //TODO ARIF I think the system is getting the first tid available in the voc. table
//            DbInterface.db_interface(result, raw_value, table_name, column_name, 'O', source, code)
//          }
//          else {
            //MULTIPLE RESULTS, BEST MATCH UNDECIDED
//            logger.info(s"""Best match undecided for value "$raw_value"""")
//            for (elem <- result_search.options){
//              val source = elem.source
//              val code = elem.code
//              val label = elem.iri
//              DbHandler.user_feedback_insert(List(expert_choice_type(default_values.int, default_values.bool, table_name, column_name, null, raw_value, null, Some(label), Some(source), Some(code), Some(Ols_interface.ols_get_iri(source,code)), "ONLINE:UNDECIDED "+result_search.score, get_timestamp())))
//            }
//          }
//        }
//        else {
//          Annotator.get_user_feedback(raw_value,table_name,column_name)
//        }
//      }
    }
  }
}
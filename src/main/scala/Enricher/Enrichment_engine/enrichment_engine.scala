package Enricher.Enrichment_engine


import Config.config
import Enricher.DBCon.{db_handler, default_values, user_feedback_type}
import org.slf4j.LoggerFactory

object enrichment_engine {

  def controller(column_name: String): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val table_name = config.get_table_by_column(column_name)
    db_handler.clean_user_feedback(table_name,column_name)
    val raw_values = db_handler.get_raw_values(table_name,column_name)
    for (raw_value <- raw_values) {
      println(column_name)
      println(raw_value)
      val result_syn = db_handler.get_cv_support_syn_by_value(raw_value.map(_.toLower))
      val result_user_changes = db_handler.get_raw_user_changes(table_name, column_name, raw_value.map(_.toLower))
      //LOCAL KB LOOKUP
      if (result_syn.tid != default_values.int || result_user_changes._1 != "null") {
        //CHECK IF TYPE IS RAW
        if (result_syn.ttype == "raw") {
          println("Value found in syn")
          logger.info(s"Value $raw_value found as RAW in local KB")
          db_handler.update_tid(raw_value, Some(result_syn.tid))
        }
        //CHECK IF RAW VALUE EXIST IN USER CHANGES
        else if (result_user_changes._1 != "null") {
          println("Value found in user changes")
          logger.info(s"Value $raw_value found in user changes")
          //GET INFO AND INSERT IN GCM
          val source = result_user_changes._1
          val a = Ols_interface.ols_get_onto_info(source)
          if (!db_handler.onto_exist(a.source)) {
            db_handler.insert_ontology(a)
          }
          db_interface.db_interface(annotator.get_info(result_user_changes._1, result_user_changes._2, raw_value, table_name, column_name), raw_value, table_name, column_name, 'U')
        }
        else if (result_syn.ttype == "syn") {
          println("Value found syn type")
          logger.info(s"Value $raw_value found as SYN in local KB")
          val suggestion = db_handler.get_cv_support_by_tid(result_syn.tid)
          if (!db_handler.user_fb_exist(raw_value, suggestion.source, suggestion.code)) {
            db_handler.user_feedback_insert(List(user_feedback_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:SYN")))
          }
        }
        else {
          println("Value found syn type")
          logger.info(s"Value $raw_value found as PREF in local KB")
          val suggestion = db_handler.get_cv_support_by_tid(result_syn.tid)
          if (!db_handler.user_fb_exist(raw_value, suggestion.source, suggestion.code)) {
            db_handler.user_feedback_insert(List(user_feedback_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:PREF")))
          }
        }
      }

      //ONLINE KB LOOKUP
      else {
        val source_code = annotator.search_term(raw_value, column_name)
        //BEST MATCH NOT FOUND
        if (source_code._1 == "") {
          println("User feedback")
          annotator.get_user_feedback(raw_value, table_name, column_name)
        }
        //BEST MATCH FOUND
        else {
          println("Best match found")
          logger.info(s"Value $raw_value best match found in online KB")
          val source = source_code._1
          val a = Ols_interface.ols_get_onto_info(source)
          if (!db_handler.onto_exist(a.source)) {
            db_handler.insert_ontology(a)
          }
          val result = annotator.get_info(source_code._1, source_code._2, raw_value, table_name, column_name)
          db_interface.db_interface(result, raw_value, table_name, column_name, 'O')
        }
      }
    }
  }
}
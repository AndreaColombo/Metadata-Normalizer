package Enrichment_engine


import DBcon.{default_values, gecotest_handler, user_feedback_type}
import Config.config.get_termtype_list

object enrichment_engine {

  def controller(table_name: String = ""): Unit = {
    val t = get_termtype_list(table_name)
    for (term_type <- t) {
      val raw_values = gecotest_handler.get_raw_values(table_name,term_type)
      for (raw_value <- raw_values) {
        println(term_type)
        println(raw_value)
        val result_syn = gecotest_handler.get_cv_support_syn_by_value(raw_value.map(_.toLower))
        val result_user_changes = gecotest_handler.get_raw_user_changes(table_name,term_type,raw_value.map(_.toLower))

        //LOCAL KB LOOKUP
        if(result_syn.tid != default_values.int || result_user_changes._1 != "null") {
          //CHECK IF TYPE IS RAW
          if (result_syn.ttype == "raw") {
            println("Value found in syn")
            gecotest_handler.update_tid(raw_value, Some(result_syn.tid))
          }
          //CHECK IF RAW VALUE EXIST IN USER CHANGES
          else if(result_user_changes._1 != "null") {
            println("Value found in user changes")
            //GET INFO AND INSERT IN GCM
            db_interface.db_interface(annotator.get_info(result_user_changes._1, result_user_changes._2), raw_value, table_name, term_type, 'U')
          }
          //if (result_syn.ttype == "syn" || result_syn.ttype == "pref"){
          else {
            println("Value found not raw type")
            val suggestion = gecotest_handler.get_cv_support_by_tid(result_syn.tid)
            gecotest_handler.user_feedback_insert(List(user_feedback_type(default_values.int,default_values.bool,table_name,term_type,Some(result_syn.tid), raw_value,null,Some(suggestion.label),Some(suggestion.source),Some(suggestion.code))))
          }
        }

        //ONLINE KB LOOKUP
        else {
          val source_code = annotator.search_term(raw_value, term_type)
          //BEST MATCH NOT FOUND
          if (source_code._1 == "") {
            println("User feedback")
            annotator.get_user_feedback(raw_value, table_name, term_type)
          }
          //BEST MATCH FOUND
          else {
            println("Best match found")
            val result = annotator.get_info(source_code._1, source_code._2)
            db_interface.db_interface(result, raw_value, table_name, term_type, 'O')
          }
        }
      }
    }
  }
}
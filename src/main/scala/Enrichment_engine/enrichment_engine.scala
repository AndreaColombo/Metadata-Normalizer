package Enrichment_engine


import DBcon.{gecotest_handler,user_feedback_type}

object enrichment_engine {
  val m = Map("biosample" -> List("tissue", "cell_line"), "donor" -> List("ethnicity", "species"), "item" -> List("platform"), "experiment_type" -> List("technique", "target", "feature"))


  def controller(table_name: String = ""): Unit = {
    val t = m.apply(table_name)
    for (term_type <- t) {
      val raw_values = gecotest_handler.get_raw_values(term_type)
      for (raw_value <- raw_values) {
        println(raw_value)
        val result_syn = gecotest_handler.get_cv_support_syn_by_value(raw_value)
        val result_user_changes = gecotest_handler.get_raw_user_changes(table_name,term_type,raw_value)

        //LOCAL KB LOOKUP
        if(result_syn.tid != -1 || result_user_changes._1 != "null") {
          //CHECK IF TYPE IS RAW
          if (result_syn.ttype == "raw")
            gecotest_handler.update_tid(raw_value, Some(result_syn.tid))
          //CHECK IF RAW VALUE EXIST IN USER CHANGES
          else if(result_user_changes._1 != "null") {
            //GET INFO AND INSERT IN GCM
            db_interface.db_interface(annotator.get_info(result_user_changes._1, result_user_changes._2), raw_value, table_name, term_type, 'U')
          }
          //if (result_syn.ttype == "syn" || result_syn.ttype == "pref"){
          else {
            val suggestion = gecotest_handler.get_cv_support_by_tid(result_syn.tid)
            gecotest_handler.user_feedback_insert(List(user_feedback_type(table_name,term_type,Some(result_syn.tid), raw_value,null,Some(suggestion.label),Some(suggestion.source),Some(suggestion.code))))
          }
        }

        //ONLINE KB LOOKUP
        else {
          val source_code = annotator.search_term(raw_value, term_type)
          //BEST MATCH NOT FOUND
          if (source_code._1 == "") {
            annotator.get_user_feedback(raw_value, table_name, term_type)
          }
          //BEST MATCH FOUND
          else {
            val result = annotator.get_info(source_code._1, source_code._2)
            db_interface.db_interface(result, raw_value, table_name, term_type, 'O')
          }
        }
      }
    }
  }
}
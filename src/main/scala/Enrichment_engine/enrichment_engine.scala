package Enrichment_engine


import DBcon.gecotest_handler

object enrichment_engine {
  val m = Map("biosample" -> List("disease", "tissue", "cell_line"), "donor" -> List("ethnicity", "species"), "item" -> List("platform"), "experiment_type" -> List("technique", "target", "feature"))


  def controller(table_name: String = ""): Unit = {
    val t = m.apply(table_name)
    for (term_type <- t) {
      val raw_values = gecotest_handler.get_raw_values(term_type)
      for (raw_value <- raw_values) {
        println(raw_value)
        //CHECK IF RAW_VALUE IS IN CV_SUPPORT_SYN WITH RAW
        val tid = is_complete(raw_value)
        var tuple = ("","")
        if(tid != -1)
          gecotest_handler.update_tid(raw_value,Some(tid))

        //CHECK IF SOURCE_CODE EXIST IN USER CHANGES
        else if({tuple=gecotest_handler.get_raw_user_changes(table_name,term_type,raw_value); !tuple._1.equals("null")}) {
          //CHECK IF TERM IS COMPLETE
          //TRUE: UPDATE TID
          val tid = is_complete(raw_value)
          if(tid != -1) {
            gecotest_handler.update_tid(raw_value, Some(tid))
          }
          //FALSE: GET INFO AND INSERT IN GCM
          else {
            db_interface.db_interface(annotator.get_info(tuple._1,tuple._2),raw_value,table_name,term_type, 'U')
          }
        }
        //NOT FOUND IN USER CHANGES
        else {
          val source_code = annotator.search_term(raw_value,term_type)
          //BEST MATCH NOT FOUND
          if(source_code._1 == ""){
            annotator.get_user_feedback(raw_value,table_name,term_type)
          }
          //BEST MATCH FOUND
          else {
            val result = annotator.get_info(source_code._1, source_code._2)
            db_interface.db_interface(result, raw_value,table_name,term_type,'O')
          }
        }
      }
    }
  }

  //RETURNS tid IF VALUE IS COMPLETE
  //-1 IF NOT
  def is_complete (value: String): Int = {
    gecotest_handler.get_raw_in_cv_support_syn(value)
  }


}

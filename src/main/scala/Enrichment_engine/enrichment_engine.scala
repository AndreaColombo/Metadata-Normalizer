package Enrichment_engine

import java.sql.BatchUpdateException

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
          gecotest_handler.update_tid(raw_value,tid)

        //CHECK IF SOURCE_CODE EXIST IN USER CHANGES
        else if({tuple=gecotest_handler.get_raw_user_changes(table_name,term_type,raw_value); !tuple._1.equals("null")}) {
          //CHECK IF TERM IS COMPLETE
          //TRUE: UPDATE TID
          val tid = is_complete(raw_value)
          if(tid != -1) {
            gecotest_handler.update_tid(raw_value, tid)
          }
          //FALSE: GET INFO AND INSERT IN GCM
          else {
            db_interface(annotator.get_info(tuple._1,tuple._2),raw_value)
          }
        }
        else {
          val source_code = annotator.search_term(raw_value,term_type)
          if(source_code._1 == ""){
            annotator.get_user_feedback(raw_value,table_name,term_type)
          }
          else {
            val result = annotator.get_info(source_code._1, source_code._2)
            db_interface(result, raw_value)
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

  def db_interface(res: List[Map[String, String]], raw_value: String = ""): Unit = {
    var cv_support: List[List[String]] = List()
    if (res.nonEmpty) {
      for (elem <- res) {
        cv_support ++= db_insert(elem)
      }
      if(raw_value.nonEmpty) {
        val label = res.head.apply("label")
        val tid = gecotest_handler.get_tid(label)
        gecotest_handler.syn_insert(List(List(tid.toString, raw_value, "raw")))
        gecotest_handler.update_tid(raw_value,tid)
      }
      insert_hyp(cv_support, res)
    }
  }

  def db_insert (elem: Map[String,String]): List[List[String]]= {
    var insert_elem: List[List[String]] = List()
    var insert_xref: List[List[String]] = List()
    var insert_syn: List[List[String]] = List()
    var support: List[List[String]] = List()

    val source = elem.apply("source")
    val code = elem.apply("code")
    val label = elem.apply("label")

    insert_elem ++= List(List(source, code, label))
    gecotest_handler.cv_support_insert(insert_elem)
    val tid = gecotest_handler.get_tid(label).toString
    support :+= List(tid,source,code,label)

    //XREF
    var xref_l:List[String] = List()
    if (elem.apply("xref") != "null")
      xref_l = elem.apply("xref").split(",").toList

    insert_xref ++= List(List(tid, source, code))

    for (xref <- xref_l if xref_l.nonEmpty) {
      val source = xref.split(":").head
      val code = xref
      insert_xref ++= List(List(tid, source, code))
    }

    try {
      gecotest_handler.xref_insert(insert_xref)
    }
    catch {
      case e: BatchUpdateException => e.getNextException.printStackTrace()
    }

    //SYN
    var syn_l:List[String] = List()
    if (elem.apply("syn") != "null")
      syn_l = elem.apply("syn").split(",").toList

    insert_syn ++= List(List(tid, label, "pref"))

    for (syn <- syn_l if syn_l.nonEmpty) {
      val label = syn
      insert_syn ++= List(List(tid, label, "syn"))
    }

    gecotest_handler.syn_insert(insert_syn)
    support
  }

  def insert_hyp(elems: List[List[String]], res:List[Map[String,String]]): Unit = {
    var result: List[List[String]] = List()
    for(elem <- elems){
      val child_tid = elem.head
      val child_code = elem(2)
      val default:Map[String,String] = Map()
      var parents = ""
      try {
        parents = res.find(a => a.apply("code") == child_code).get.apply("parents")
      }
      catch {
        case e: NoSuchElementException =>
          res.foreach(println)
          elems.foreach(println)
          println(child_code)
          sys.exit(-1)
      }
      if(parents!=null) {
        for(parent <- parents.split(",")) {
          val parent_tid = elems.find(a => a(2) == parent).getOrElse(List("null")).head
          if(parent_tid != "null")
            result :+= List(parent_tid, child_tid, "is_a")
        }
      }

      parents = res.find(a => a.apply("code")==child_code).get.apply("part_of")
      if(parents!=null) {
        for(parent <- parents.split(",")) {
          val parent_tid = elems.find(a => a(2) == parent).getOrElse(List("null")).head
          if(parent_tid != "null")
            result :+= List(parent_tid, child_tid, "part_of")
        }
      }
    }
    try {
      gecotest_handler.hyp_insert(result)
    }
    catch  {
      case e: BatchUpdateException => e.getNextException.printStackTrace()
    }
  }

  def unfold_hyp(): Unit = {

  }
}

package Enrichment_engine

import java.sql.BatchUpdateException

import DBcon.gecotest_handler
import DBcon.gecotest_handler.cv_support_raw_type

object db_interface {

  case class cv_support(tid: String, source: String, code: String, label: String)

  def db_interface(res: List[Map[String, String]], raw_value: String = "", table_name: String, column_name: String, method: Char): Unit = {
    var cv_support: List[cv_support] = List()
    if (res.nonEmpty) {
      for (elem <- res) {
        cv_support ++= db_insert(elem)
      }
      val label = res.head.apply("label")
      val tid = gecotest_handler.get_tid(label)
      gecotest_handler.syn_insert(List(List(tid.toString, raw_value, "raw")))
      gecotest_handler.raw_insert(List(cv_support_raw_type(tid,label,table_name,column_name,method)))
      gecotest_handler.update_tid(raw_value,Some(tid))
      insert_hyp(cv_support, res)
    }
  }

  //INSERT ELEMENTS IN cv_support, syn, xref
  def db_insert (elem: Map[String,String]): List[cv_support]= {
    var insert_elem: List[List[String]] = List()
    var insert_xref: List[List[String]] = List()
    var insert_syn: List[List[String]] = List()
    var support: List[cv_support] = List()

    val source = elem.apply("source")
    val code = elem.apply("code")
    val label = elem.apply("label")
    val description = elem.apply("description")

    insert_elem ++= List(List(source, code, label, description))
    gecotest_handler.cv_support_insert(insert_elem)
    val tid = gecotest_handler.get_tid(label).toString
    support :+= cv_support(tid,source,code,label)

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
      case e: BatchUpdateException => insert_xref.foreach(println)
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

  def insert_hyp(elems: List[cv_support], res:List[Map[String,String]]): Unit = {
    var result: List[List[String]] = List()
    val default_cv: cv_support = cv_support("null","null","null","null")
    for(elem <- elems){
      val child_tid = elem.tid
      val child_code = elem.code
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
          val parent_tid = elems.find(a => a.code == parent).getOrElse(default_cv).tid
          if(parent_tid != "null")
            result :+= List(parent_tid, child_tid, "is_a")
        }
      }

      parents = res.find(a => a.apply("code")==child_code).get.apply("part_of")
      if(parents!=null) {
        for(parent <- parents.split(",")) {
          val parent_tid = elems.find(a => a.code == parent).getOrElse(default_cv).tid
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

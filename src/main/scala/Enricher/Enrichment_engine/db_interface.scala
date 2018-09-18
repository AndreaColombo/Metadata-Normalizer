package Enricher.Enrichment_engine

import java.sql.BatchUpdateException

import Enricher.DBCon._
import org.slf4j.{Logger, LoggerFactory}


object db_interface {

  case class cv_support(tid: String, source: String, code: String, label: String)

  def db_interface(res: List[Map[String, String]], raw_value: String, table_name: String, column_name: String, method: Char): Unit = {
    var cv_support: List[cv_support] = List()
    if (res.nonEmpty) {
      for (elem <- res) {
        cv_support ++= db_insert(elem)
      }
      val label = res.head.apply("label")
      val tid = db_handler.get_tid(res.head.apply("source"),res.head.apply("code"))
      db_handler.syn_insert(List(synonym_type(tid,raw_value,"raw")))
      db_handler.raw_insert(List(raw_annotation_type(tid,raw_value,table_name,column_name,method)))
      db_handler.update_tid(raw_value,Some(tid))
      insert_hyp(cv_support, res)
      unfold_hyp(tid)
    }
  }

  //INSERT ELEMENTS IN vocabulary, syn, xref
  def db_insert (elem: Map[String,String]): List[cv_support]= {
    var insert_elem: List[vocabulary_type] = List()
    var insert_xref: List[reference_type] = List()
    var insert_syn: List[synonym_type] = List()
    var support: List[cv_support] = List()

    val source = elem.apply("source")
    val code = elem.apply("code")
    val label = elem.apply("label")
    val description = elem.apply("description")
    val iri = elem.apply("iri")

    //CHECK EXISTENCE OF SOURCE IN ONTOLOGY TABLE
    //IF DOESN'T EXIST INSERT

    insert_elem ++= List(vocabulary_type(default_values.int,source,code,label,description,iri))
    db_handler.cv_support_insert(insert_elem)
    val tid = db_handler.get_tid(source,code)
    support :+= cv_support(tid.toString,source,code,label)

    //XREF
    var xref_l:List[String] = List()

    if (elem.apply("xref") != "null")
      xref_l = elem.apply("xref").split(",").toList

    insert_xref ++= List(reference_type(tid, source, code))

    for (xref <- xref_l if xref_l.nonEmpty) {
      val source = xref.split(":").head
      val code = xref
      insert_xref ++= List(reference_type(tid, source, code))
    }
    db_handler.xref_insert(insert_xref)
    //END XREF

    //SYN
    var syn_l:List[String] = List()
    if (elem.apply("syn") != "null")
      syn_l = elem.apply("syn").split(",").toList

    insert_syn ++= List(synonym_type(tid, label, "pref"))

    for (syn <- syn_l if syn_l.nonEmpty) {
      val label = syn
      insert_syn ++= List(synonym_type(tid, label, "syn"))
    }

    db_handler.syn_insert(insert_syn)
    //END SYN

    support
  }

  def insert_hyp(elems: List[cv_support], res:List[Map[String,String]]): Unit = {
    var result: List[relationship_type] = List()
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
            result :+= relationship_type(Integer.parseInt(parent_tid), Integer.parseInt(child_tid), "is_a")
        }
      }

      parents = res.find(a => a.apply("code")==child_code).get.apply("part_of")
      if(parents!=null) {
        for(parent <- parents.split(",")) {
          val parent_tid = elems.find(a => a.code == parent).getOrElse(default_cv).tid
          if(parent_tid != "null")
            result :+= relationship_type(Integer.parseInt(parent_tid), Integer.parseInt(child_tid), "part_of")
        }
      }
    }
    try {
      db_handler.hyp_insert(result)
    }
    catch  {
      case e: BatchUpdateException => e.getNextException.printStackTrace()
    }
  }

  def unfold_hyp(cur_tid: Int): Unit = {
    val tid_list = db_handler.get_tid_parent_distinct(cur_tid)
    var unfolded: List[relationship_unfolded_type] = List()
    for (tid_parent_cur <- tid_list) {
      val distance = 1
      val onto_support_hyp_l = db_handler.get_onto_hyp(tid_parent_cur)
      for (onto_support_hyp <- onto_support_hyp_l){
        unfolded :+= relationship_unfolded_type(onto_support_hyp.tid_p,onto_support_hyp.tid_c,distance,onto_support_hyp.rel_type)
        unfolded ++= unfold_recursive(tid_parent_cur,onto_support_hyp.tid_c,distance+1,onto_support_hyp.rel_type)
      }
    }
    //Remove duplicates and then keeps only relations with minimum distance
    //EG if (5,10,dist=1) exists then (5,10,dist=2) is eliminated
    unfolded = unfolded.distinct.filterNot(a => unfolded.exists(p => p.tid_a == a.tid_a && p.tid_d == a.tid_d && p.distance < a.distance))
    db_handler.insert_unfolded(unfolded)
  }

  def unfold_recursive(tid_parent_cur: Int, tid_child_cur: Int, distance: Int, rel_type_cur: String): List[relationship_unfolded_type] = {
    var result: List[relationship_unfolded_type] = List()
    val onto_support_hyp_l = db_handler.get_onto_hyp(tid_child_cur)

    for (elem <- onto_support_hyp_l) {
      val rel_type = get_rel_type(rel_type_cur,elem.rel_type)
      result :+= relationship_unfolded_type(tid_parent_cur,elem.tid_c,distance,rel_type)
      result ++= unfold_recursive(tid_parent_cur,elem.tid_c,distance+1,rel_type)
    }
    result
  }

  def get_rel_type(rel_type1: String, rel_type2: String):String = if(rel_type1.equals(rel_type2)) rel_type1 else "mixed"
}




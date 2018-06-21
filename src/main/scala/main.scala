import java.io.File
import java.net.URLDecoder
import java.sql.BatchUpdateException
import java.util.{Calendar, Date}

import DBcon._
import user_selection._
import com.github.tototoshi.csv._
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"
  var i = 0

  override def main(args: Array[String]): Unit = {
    gecotest_handler.init()
    val m = Map("biosample" -> List("disease", "tissue", "cell_line"), "donor" -> List("ethnicity", "species"), "item" -> List("platform"), "experiment_type" -> List("technique", "target", "feature"))
    var res: List[Map[String, String]] = List()
    if(args(0).equalsIgnoreCase("user") && args(1).equalsIgnoreCase("selection"))
      user_selection.get_user_selection()
    else {
      val t = m.apply(args(0))
      for (term_type <- t) {
        val term_l = gecotest_handler.get_raw_values(term_type)
        for (term <- term_l) {
          println(term)
          var cv_support: List[List[String]] = List()
          res = annotator.get_annotation(term, args(0), term_type)
          if (res.nonEmpty) {
            for (elem <- res) {
              cv_support ++= insert_cose(elem, term)
            }
            val label = res.head.apply("label")
            val tid = gecotest_handler.get_tid(label)
            gecotest_handler.syn_insert(List(List(tid.toString, term, "raw")))
            get_parents(cv_support, res)
          }
        }
      }
    }
  }

  def read(table: String): List[List[String]] = {
    val f = new File(""+table+".csv")
    val reader = CSVReader.open(f)
    var elem: List[List[String]] = reader.all()
    elem
  }

  def insert_cose (elem: Map[String,String],term: String): List[List[String]]= {
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
    support :+= List(tid, source,code,label)

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

  def get_parents(elems: List[List[String]],res:List[Map[String,String]]): Unit = {
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

  def get_elapsed_time(d1: Long, d2: Long) = {
    val elapsed:Double = (d2-d1).toDouble / 1000
    val min: Double = (elapsed / 60).intValue()
    val sec: Double = (((elapsed / 60) - min) * 60).intValue
    val millis = ((((elapsed / 60) - min) * 60) - sec) * 1000
    println(min.toInt + ":" + sec.toInt + ":" + millis.toInt)
  }

  def get_timestamp() = {
    val now = Calendar.getInstance()
    println(now.get(Calendar.HOUR_OF_DAY)+":"+now.get(Calendar.MINUTE)+":"+now.get(Calendar.SECOND))
  }
}
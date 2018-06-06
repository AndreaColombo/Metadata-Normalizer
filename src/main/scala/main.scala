import java.io.File
import java.net.URLDecoder
import java.util.{Calendar, Date}

import DBcon._
import com.github.tototoshi.csv._
import play.api.libs.json.Json
import scalaj.http.{Http, HttpOptions}


object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"

  var res: List[Map[String,String]] = List()

  var i = 0

  override def main(args: Array[String]): Unit = {

    val m = Map("biosample" -> List("disease", "tissue", "cell_line"), "donor" -> List("ethnicity","species"), "item" -> List("platform"), "experiment_type" -> List("technique","target","feature"))//, "container" -> List("annotation"))

    val d1 = System.currentTimeMillis()


    val term = "liver"
    res = annotator.get_annotation(term,"uberon")
    for (elem <- res)
      insert_cose(elem)
    get_parents()
  }

  def insert(elem: List[List[String]], table: String, append: Boolean=true) = {
    val f = new File(""+table+".csv")
    val writer = CSVWriter.open(f, append)
    writer.writeAll(elem)
  }

  def read(table: String): List[List[String]] = {
    val f = new File(""+table+".csv")
    val reader = CSVReader.open(f)
    var elem: List[List[String]] = reader.all()
    elem
  }

  def insert_cose (elem: Map[String,String]) = {
    var insert_elem: List[List[String]] = List()
    var insert_xref: List[List[String]] = List()
    var insert_syn: List[List[String]] = List()
    var insert_parents: List[List[String]] = List()

    val source = elem.apply("source")
    val code = elem.apply("code")
    val label = elem.apply("label")
    var tid = get_tid()

    insert_elem ++= List(List(tid, source, code, label))


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

    //SYN
    var syn_l:List[String] = List()
    if (elem.apply("syn") != "null")
      syn_l = elem.apply("syn").split(",").toList

    insert_syn ++= List(List(tid, label))

    for (syn <- syn_l if syn_l.nonEmpty) {
      val label = syn
      insert_syn ++= List(List(tid, label))
    }

    insert(insert_elem, "cv_support")
    insert(insert_xref, "cv_support_xref")
    insert(insert_syn, "cv_support_syn")
  }

  def get_parents() = {
    val elems = read("cv_support")
    var result: List[List[String]] = List()
    for(elem <- elems){
      val child_tid = elem.head
      val child_code = elem(2)
      val default:Map[String,String] = Map()

      var parents = res.find(a => a.apply("code")==child_code).get.apply("parents")
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
    insert(result,"onto_support_hyp",false)
  }

  def get_tid(): String = {
    val tmp = i
    i += 1
    tmp.toString
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
package Utilities

import java.io._

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

object Utils {

  def escape (str: String) : String = {
    str.replace('/', '-').replace('\\','-').replace(':','-').replace(';','-').replace('*','-').replace('?','-').replace('"','-').replace("--", "-")
  }

 def write_to_file (str: String, path_raw: String, filename: String, append: Boolean = false) : Boolean = {
   var path = ""
   if(path_raw.last == '/'){
     path = path_raw+escape(filename)
   }
   else {
     path = path_raw + "/" + escape(filename)
   }
   var writer: FileWriter = null
   var pw: PrintWriter = null
   var bw: BufferedWriter = null
   try {
     writer = new FileWriter(path)
     pw = new PrintWriter(writer)
     bw = new BufferedWriter(writer)
    }
    catch {
      case e: FileNotFoundException =>
        e.printStackTrace()
        return false

      case e1: IOException =>
        e1.printStackTrace()
        return false

    }
    if (append){
      pw.println(str)
    }
    else {
      writer.write(str)
      writer.flush()
    }
    writer.close()
    true
  }

  def get_timestamp(): String = {
    DateTime.now.toString(DateTimeFormat.forPattern("yyyy_MM_dd_HH_mm_ss_SSS"))
  }

  //ELIMINATE DUPLICATES FROM LIST BASED ON CONDITIONS
  def distinctBy[L, E](list: List[L])(f: L => E): List[L] =
    list.foldLeft((Vector.empty[L], Set.empty[E])) {
      case ((acc, set), item) =>
        val key = f(item)
        if (set.contains(key)) (acc, set)
        else (acc :+ item, set + key)
    }._1.toList
}

import scala.collection.mutable.ListBuffer
import scala.reflect.internal.util.StringOps

object Preprocessing {
  def parse(input: String): String = {
    //NEWLINE = VIRGOLA
    val tmp = input.replace(""""""", "").replace("\n", ",").replace("""\""", "").replace("/", ",").replace(";", ",")
    //tolgo virgolette, punti e virgola e new line
    var result = ""
    var result2 = ""
    var dropped = ""
    val lst = tmp.split(",").toList
    for (s <- lst) {
      val res = line_parse(s)
      if (res.startsWith("DROP"))
        dropped += res.replace("DROP ","") + ","
      else {
        val tmp = res.replaceAll("\\s*,\\s*", ",")
        result += tmp + ","
      }
    }
    dropped = dropped.dropRight(1)
    result = result.dropRight(1)
    result.split(",").toList.distinct.foreach(result2+= _ + ",")

    //prima del return crea new list con elementi DROP
    return result2.dropRight(1)
  }

  def line_parse(s: String): String = {
    //if s non ci piace prepend DROP
    var str = ""
    if (s.startsWith(" "))
      str = s.drop(1)
    else str = s
    str = str.map(_.toLower)

    //STARTS WITH NUMBER -> DROP
    val n = List("0,","1","2","3","4","5","6","7","8","9","+","-",";")
    if(n.exists(token => str.startsWith(token))){
      str = drop(str)
    }

    if(!is_balanced(str.toList) && !str.startsWith("DROP")) {
      str = balance(str)
    }


    //TXT (TXT2) => TXT, TXT2
    val r = "\\((.*?)\\)".r
    if (!r.findAllIn(str).mkString.equals("") && !str.startsWith("DROP"))
      str = str.replace(r.findAllIn(str).mkString, ","+r.findAllIn(str).mkString.replace("(","").replace(")",""))
    if (str.startsWith(",")) str = str.drop(1)

    //FILTER OUT ALL NUMBER SEQUENCES
//    val r2 = "([0-9]+)".r
//    str = str.replace(r2.findAllIn(str).mkString,"")

    val stopwords = List("from", "for")
    for (s <- stopwords){
      str = str.replace(s, ",")
    }

    //FILTER IDS LIKE letters+numbers
    val r2 = "([a-z])([0-9]+)".r
//    if (!r2.findAllIn(str).mkString.isEmpty)
//      println(r2.findAllIn(str).mkString + " found in "+str)

//    println(str)
    return ltrim(rtrim(str))

  }

  def drop(str: String): String = return "DROP " + str

  def balance(chars: String): String = {
    var str = ""
    if (chars.contains("(")) //c'è ( metto ) in coda
      str = chars+")"
    else if (chars.contains(")")) // c'è ) metto ( all'inizio
      str = "("+chars
    else str = chars
    return str
  }

  def is_balanced(chars: List[Char]): Boolean = {
    def f(chars: List[Char], numOpens: Int): Boolean = {
      if (chars.isEmpty) {
        numOpens == 0
      } else {
        val h = chars.head
        val n =
          if (h == '(') numOpens + 1
          else if (h == ')') numOpens - 1
          else numOpens
        if (n >= 0) f(chars.tail, n)
        else false
      }
    }

    f(chars, 0)
  }

  private def ltrim(s: String) = s.replaceAll("^\\s+", "")
  private def rtrim(s: String) = s.replaceAll("\\s+$", "")

}
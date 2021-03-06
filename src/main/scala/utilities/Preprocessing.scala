package utilities

/**
  * This object contains all methods used for the preprocessing of a raw value
  */
object Preprocessing {
  var lookup_map: Map[String, Seq[String]] = Map()

  /**
    * Transform the parsed value into the raw value from which it originated, done via a lookup map
    * @param term parsed value to map
    * @return original raw value
    */
  def lookup (term: String): String = {
    var originalValue = ""
    val default = (-1,"")
    lookup_map.find(_._2.contains(term)).getOrElse(default)._1.toString
  }

  /**
    * Transform a list of raw values into a list of parsed values
    * @param input List of raw values to parse
    * @return List of parsed values
    */
  def parse(input: List[String]): List[String] = {
    var result = ""
    for (s <- input){
      val raw_value = s
      var tmp = ""
      val raw_input = raw_value
        .replace(""""""", "")
        .replace("""\""", "")
        .replace(";", ",")
        .split(",")

      for (a <- raw_input){
        val medium_value = line_parse(line_parse(a))
        if (!medium_value.startsWith("drop")) {
          val parsed_value = trim(inner_parse(medium_value))
          tmp += parsed_value + ","
        }
      }
      val parsedValue = tmp.replaceAll("\\s*,\\s*", ",").replaceAll("  "," ").dropRight(1)
      lookup_map += (raw_value -> parsedValue.split(","))
      result += parsedValue+","
    }
    result.dropRight(1).replaceAll(",,",",").split(",").toList
  }

  /**
    * First parsing of raw value, filters texts that aren't meaningful for the purpose of our application
    * @param s Text to parse
    * @return
    */
  def line_parse(s: String): String = {
    //if s non ci piace prepend DROP
    var str = ""
    if (s.startsWith(" "))
      str = s.drop(1)
    else str = s
    str = str.map(_.toLower)
    //STARTS WITH NUMBER -> DROP
    val n = List("0,", "1", "2", "3", "4", "5", "6", "7", "8", "9", "+", "-", ";")
    if (n.exists(token => str.startsWith(token))) {
      str = drop(str)
    }
    val tmp = str.replace("(","").replace(")","")
    if ((tmp.length <= 3 && tmp.contains(" ")) || tmp.length <= 2) {
      str = drop(str)
    }

    if (!is_balanced(str.toList) && !str.startsWith("DROP")) {
      str = balance(str)
    }
    str
  }

  /**
    * Final parsing of raw value, parses text according to various rules
    * @param s Text to parse
    * @return
    */
  def inner_parse(s: String): String = {
    //TXT (TXT2) => TXT, TXT2, TXT TXT2
    var str = s
    val r = "\\((.*?)\\)".r
    if (!r.findAllIn(str).mkString.equals("") && !str.startsWith("DROP")) {
      str = str.replace(r.findAllIn(str).mkString, "," + r.findAllIn(str).mkString.replace("(", "").replace(")", ""))
      str = str + "," + str.replace(","," ")
    }
    if (str.startsWith(",")) str = str.drop(1)

    //STOPWORDS
    // txt STOPWORD txt2 = txt, txt2, txt txt2

    val stopwords = List(" from ", " for "," and ")
    if(stopwords.exists(stopword => str.contains(stopword))) {
      stopwords.foreach(stopword => str = str.replaceAll(stopword, ","))
      str = str + "," + str.replace(","," ")
      //negative,hiv-1,
      //
    }

    //TRATTINI
    if(str.contains("-")){
      str += "," + str.replace("-"," ")
    }

    if(str.contains("/")){
      str += "," + str.replace("/"," ")
    }
    trim(s)
  }

  /**
    * Prepend drop to a string so that the program knows to ignore it
    * @param str String to drop
    * @return
    */
  def drop(str: String): String = "DROP " + str

  /**
    * Balance parentheses in a string
    * @param chars String to balance parentheses
    * @return
    */
  def balance(chars: String): String = {
    var str = ""
    if (chars.contains("(")) //c'è ( metto ) in coda
      str = chars+")"
    else if (chars.contains(")")) // c'è ) metto ( all'inizio
      str = "("+chars
    else str = chars
    str
  }

  /**
    * Returns true if string is parentheses balanced, false otherwise
    * @param chars String to check
    * @return
    */
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

  def trim (s: String) = ltrim(rtrim(s))
  /**
    * Eliminate blank spaces at the beginning of the string
    * @param s String to clean
    * @return
    */
  private def ltrim(s: String) = s.replaceAll("^\\s+", "")

  /**
    * Eliminate blank spaces at the end of the string
    * @param s String to clean
    * @return
    */
  private def rtrim(s: String) = s.replaceAll("\\s+$", "")

}
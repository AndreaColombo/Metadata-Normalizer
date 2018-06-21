import DBcon.gecotest_handler
import scalaj.http.{Http, HttpOptions}

import scala.io._
import scalax.cli._
import shapeless._

object user_selection {

  val m = Map("biosample" -> List("disease", "tissue", "cell_line"), "donor" -> List("ethnicity", "species"), "item" -> List("platform"), "experiment_type" -> List("technique", "target", "feature"))

  def get_user_selection(): Unit = {
    for (table_name <- m.keys){
      for (column_name <- m.apply(table_name)){
        val raw_values = gecotest_handler.get_user_feedback_raw_values(table_name,column_name)
        for (rv <- raw_values){
          var i = 0
          val options = gecotest_handler.get_user_feedback_infos(rv)
          val table = Table(Sized("id","table name", "column name", "raw value", "parsed value","label","source","code"))
          for (o <- options){
            table.rows += Sized(i.toString,o._1,o._2,o._3,o._4,o._5,o._6,o._7)
            i+=1
          }
          table.alignments
          //CASE RAW VALUE NOT FOUND IN OLS LOOKUP
          if(options.head._7=="null"){
            table.print()
            println("Please input manually source and code")
            val user_choice = input_source_code()
            val source = user_choice._1
            val code = user_choice._2
            val prefLabel = annotator.get_info(code,source).head(2)
            //INSERT IN USER REQUESTED CHOICE
            gecotest_handler.insert_user_changes(table_name, column_name, rv, source, code, prefLabel)
          }
          //CASE RAW VALUE FOUND BUT NOT BEST MATCH
          else {
            table.print()
            val user_choice = get_input(i)
            if(user_choice.equalsIgnoreCase("manual insert")){
              println("Please input manually source and code")
              val user_choice = input_source_code()
              val source = user_choice._1
              val code = user_choice._2
              val prefLabel = annotator.get_info(code,source).head(2)
              //INSERT IN USER REQUESTED CHOICE
              gecotest_handler.insert_user_changes(table_name, column_name, rv, source, code, prefLabel)
            }
            else {
              val a = options(user_choice.toInt)
              gecotest_handler.insert_user_changes(a._1,a._2,a._3,a._6,a._7,a._5)
            }
          }
        }
      }
    }
  }

  def get_input(range: Int): String = {
    display_prompt()
    var user_choice = StdIn.readLine()
    var id_choice = -1
    try id_choice = Integer.parseInt(user_choice)
    catch {
      case e: NumberFormatException => e
    }
    while (!user_choice.equalsIgnoreCase("manual insert") && ((id_choice<0) || (id_choice>range))) {
      println("Unknown command")
      display_prompt()
      user_choice = StdIn.readLine()
      try id_choice = Integer.parseInt(user_choice)
      catch {
        case e: NumberFormatException => e
      }
    }
    user_choice
  }

  def input_source_code(): (String, String) = {

    println("Please input source")
    var input = StdIn.readLine()
    while(!validate_source(input)) {
      println("Error, source not valid")
      println("Please input a valid source")
      input = StdIn.readLine()
      println(!validate_source(input))
    }
    val source = input

    println("Please input code in the form ONTO_XXXXXXXX")
    input = StdIn.readLine()
    while(!validate_code(source, input.map(_.toUpper))) {
      println("Error, code not valid")
      println("Please input a valid code")
      input = StdIn.readLine()
    }
    val code = input.map(_.toUpper)

    println()
    (source,code)
  }

  def display_prompt(): Unit = {
    println("SELECT ID")
    println("or")
    println("MANUAL INSERT")
    println()
  }

  //return true if source is valid
  def validate_source(source: String): Boolean = {
    Http("https://www.ebi.ac.uk/ols/api/ontologies/"+source).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get.contains("200")
  }

  //return true if code is valid
  def validate_code(onto: String, code: String): Boolean = {
    Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$onto/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F" + code).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get.contains("200")
  }
}
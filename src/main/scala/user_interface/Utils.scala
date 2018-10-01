package user_interface

import config_pkg.ApplicationConfig
import enricher.engine.Ols_interface
import scalaj.http.{Http, HttpOptions}

import scala.io.StdIn

object Utils {
  def choose_table_column(): (String, String) = {
    println("Choose table")
    val table_list = ApplicationConfig.get_gcm_table_list()
    for (i <- table_list.indices){
      println(i+1 + " - " + table_list(i))
    }
    val table = table_list(get_choice(table_list.length).toInt-1)

    println("Choose column")
    val column_list = ApplicationConfig.get_termtype_list(table)
    for (i <- column_list.indices){
      println(i+1 + " - " + column_list(i))
    }
    val column = column_list(get_choice(column_list.length).toInt-1)

    (table,column)
  }

  def get_choice(range: Int): String = {
    var choice = ""
    var user_choice = StdIn.readLine()
    var id_choice = -1
    try id_choice = Integer.parseInt(user_choice)
    catch {
      case e: NumberFormatException => e
    }
    while (id_choice <= 0 || id_choice > range){
      println("Invalid command")
      user_choice = StdIn.readLine()
      try id_choice = Integer.parseInt(user_choice)
      catch {
        case e: NumberFormatException => e
      }
    }
    choice = id_choice.toString
    choice
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

  //return true if source is valid
  def validate_source(source: String): Boolean = {
    Http("https://www.ebi.ac.uk/ols/api/ontologies/"+source).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get.contains("200")
  }

  //return true if iri is valid
  def validate_code(onto: String, code: String): Boolean = {
    val iri = Ols_interface.ols_get_iri(onto,code)
    Ols_interface.ols_get_status(onto,iri).contains("200")
  }
}

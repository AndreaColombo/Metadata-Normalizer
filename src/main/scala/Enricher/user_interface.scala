package Enricher

import Config.config.{get_gcm_table_list, get_termtype_list}
import Enricher.DBCon.{default_values, db_handler, user_changes_type}
import Enrichment_engine.annotator
import scalaj.http.{Http, HttpOptions}
import scalax.cli.Table
import shapeless.Sized

import scala.io.StdIn

object user_interface {

  def revision_routine(): Unit = {
    var flag = true
    while (flag) {
      println("Input value to update")
      var value = StdIn.readLine()
      while (db_handler.get_value_info(value).isEmpty){
        println("Value not valid")
        println("Input valid value")
        value = StdIn.readLine()
      }
      val source_code = input_source_code()
      val tuples = db_handler.update_tid(value,null)

      for ((table_name, column_name) <- tuples) {
        db_handler.insert_user_changes(user_changes_type(default_values.int,table_name, column_name, value, source_code._1, source_code._2))
      }

      println("Do you wish to update another value?")
      println("y/n")

      var choice = StdIn.readLine()
      while (!choice.equalsIgnoreCase("y") && !choice.equalsIgnoreCase("n")){
        println("unknown command")
        println("type y/n")

        choice = StdIn.readLine()
      }

      if(choice.equalsIgnoreCase("y")){
        flag = true
      }
      else if(choice.equalsIgnoreCase("n"))
        flag = false
    }
  }

  def get_user_feedback(): Unit = {
    for (table_name <- get_gcm_table_list()){
      for (column_name <- get_termtype_list(table_name)){
        val raw_values = db_handler.get_user_feedback_raw_values(table_name,column_name)
        for (rv <- raw_values){
          var i = 0
          val options = db_handler.get_user_feedback_infos(rv)
          val table = Table(Sized("id","table name", "column name", "raw value", "parsed value","label","source","iri"))
          for (o <- options){
            table.rows += Sized(i.toString,o.table,o.column,o.raw_value,o.parsed_value.get,o.label.get,o.source.get,o.code.get)
            i+=1
          }
          table.alignments
          //CASE RAW VALUE NOT FOUND IN OLS LOOKUP
          if(!options.head.code.isDefined){
            table.print()
            println("Please input manually source and iri")
            val user_choice = input_source_code()
            val source = user_choice._1
            val code = user_choice._2
            val prefLabel = annotator.ols_get_info(code,source).head(2)
            //INSERT IN USER REQUESTED CHOICE
            db_handler.insert_user_changes(user_changes_type(default_values.int,table_name, column_name, rv, source, code))
          }
          //CASE RAW VALUE FOUND BUT NOT BEST MATCH
          else {
            table.print()
            val user_choice = get_input(i)
            if(user_choice.equalsIgnoreCase("manual apiresults_insert")){
              println("Please input manually source and iri")
              val user_choice = input_source_code()
              val source = user_choice._1
              val code = user_choice._2
              val prefLabel = annotator.ols_get_info(source,code).head(2)
              //INSERT IN USER REQUESTED CHOICE
              db_handler.insert_user_changes(user_changes_type(default_values.int,table_name, column_name, rv, source, code))
            }
            else {
              val a = options(user_choice.toInt)
              db_handler.insert_user_changes(user_changes_type(default_values.int,a.table,a.column,a.raw_value,a.source.get,a.code.get))
            }
          }
          db_handler.set_resolved(rv)
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
    while (!user_choice.equalsIgnoreCase("manual apiresults_insert") && ((id_choice<0) || (id_choice>range))) {
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

    println("Please input iri in the form ONTO_XXXXXXXX")
    input = StdIn.readLine()
    while(!validate_code(source, input.map(_.toUpper))) {
      println("Error, iri not valid")
      println("Please input a valid iri")
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

  //return true if iri is valid
  def validate_code(onto: String, code: String): Boolean = {
    Http(s"https://www.ebi.ac.uk/ols/api/ontologies/$onto/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252F" + code).option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString.header("status").get.contains("200")
  }
}

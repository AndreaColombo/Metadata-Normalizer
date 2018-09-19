package user_interface

import Config.config.{get_gcm_table_list, get_termtype_list}
import Enricher.DBCon.{db_handler, default_values, expert_preference_type}
import Enricher.Enrichment_engine.Ols_interface
import scalaj.http.{Http, HttpOptions}
import scalax.cli.Table
import shapeless.Sized
import scala.util.control.Breaks._

import scala.io.StdIn

object Expert_preference {
  val MANUAL_INSERT_RANGE = 3
  val POSSIBLE_CHOICES_RANGE = 4

  def get_user_feedback(): Unit = {
    for (table_name <- get_gcm_table_list()){
      for (column_name <- get_termtype_list(table_name)){
        val raw_values = db_handler.get_user_feedback_raw_values(table_name,column_name)
        for (rv <- raw_values){
          println("Table: "+table_name)
          println("Column: "+column_name)
          println("Raw value: "+rv)
          println()
          var i = 0
          val options = db_handler.get_user_feedback_infos(rv)
          val table = Table(Sized("id","parsed value","pref_label","source","code","iri"))
          for (o <- options){
            table.rows += Sized((i+1).toString,o.parsed_value.getOrElse("null"),o.label.getOrElse("null"),o.source.getOrElse("null"),o.code.getOrElse("null"),o.iri.getOrElse("null"))
            i+=1
          }
          table.alignments
          //CASE RAW VALUE NOT FOUND IN OLS LOOKUP
          if(!options.head.code.isDefined){
            table.print()
            println()
            display_prompt(false)
            val user_choice = get_choice(MANUAL_INSERT_RANGE)
            //1 MANUAL INSERT
            //2 SKIP
            //3 BACK
            if (user_choice.equals("1")){
              val user_sourcecode = input_source_code()
              val source = user_sourcecode._1
              val code = user_sourcecode._2
              val prefLabel = Ols_interface.ols_get_info(source,code).head(2)
              //INSERT IN USER REQUESTED CHOICE
              db_handler.insert_user_changes(expert_preference_type(default_values.int,table_name, column_name, rv, source, code))
              db_handler.set_resolved(rv,table_name,column_name)
            }
            else if (user_choice.equals("2")){
              breakable {
                break()
              }
            }
            else if (user_choice.equals("3")){
              println("Returning to home page")
              println()
              println()
              return
            }
          }
          //CASE RAW VALUE FOUND BUT NOT BEST MATCH
          else {
            table.print()
            println()
            display_prompt(true)
            val user_choice = get_choice(POSSIBLE_CHOICES_RANGE)
            if(user_choice.equals("1")){
              println("Please input manually source and code")
              val user_choice = input_source_code()
              val source = user_choice._1
              val code = user_choice._2
              val prefLabel = Ols_interface.ols_get_info(source,code).head(2)
              //INSERT IN USER REQUESTED CHOICE
              db_handler.insert_user_changes(expert_preference_type(default_values.int,table_name, column_name, rv, source, code))
              db_handler.set_resolved(rv,table_name,column_name)
            }
            else if (user_choice.equals("2")){
              println("Please select an ID")
              val user_selection = get_choice(options.length)
              val a = options(user_selection.toInt-1)
              db_handler.insert_user_changes(expert_preference_type(default_values.int,a.table,a.column,a.raw_value,a.source.get,a.code.get))
              db_handler.set_resolved(rv,table_name,column_name)
            }
            else if (user_choice.equals("3")){
              breakable {
                break()
              }
            }
            else if (user_choice.equals("4")){
              println("Returning to home page")
              println()
              println()
              return
            }
          }
        }
      }
    }
    println("Returning to home page")
    println()
    println()
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

  def display_prompt(flag: Boolean): Unit = {
    println("1 - MANUAL INSERT")
    if(flag) {
      println("2 - SELECT ID")
      println("3 - SKIP")
      println("4 - BACK")
    }
    else {
      println("2 - SKIP")
      println("3 - BACK")
    }

    println()
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

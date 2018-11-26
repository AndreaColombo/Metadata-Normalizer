package user_interface

import config_pkg.ApplicationConfig.{get_gcm_table_list, get_termtype_list}
import enricher.dbcon.{DbHandler, default_values, expert_preference_type}
import enricher.engine.OlsInterface
import Utils._
import scalax.cli.Table
import shapeless.Sized
import scala.util.control.Breaks._

object ExpertPreference {
  val MANUAL_INSERT_RANGE = 3
  val POSSIBLE_CHOICES_RANGE = 4

  def get_user_feedback(): Unit = {
    for (table_name <- get_gcm_table_list()){
      for (column_name <- get_termtype_list(table_name)){
        val raw_values = DbHandler.get_user_feedback_raw_values(table_name,column_name)
        for (rv <- raw_values){
          var i = 0
          val options = DbHandler.get_user_feedback_infos(rv)
          val table = Table(Sized("id","parsed value","label","ontology","code","iri"))
          for (o <- options){
            //            table.rows += Sized((i+1).toString,o.parsed_value.getOrElse("null"),o.label.getOrElse("null"),o.source.getOrElse("null"),o.code.getOrElse("null"),o.iri.getOrElse("null"))
            table.rows += Sized((i+1).toString,o.parsed_value.getOrElse("null"),o.label.getOrElse("null"),o.source.getOrElse("null"),o.code.getOrElse("null"),"...")
            i+=1
          }
          table.alignments
          println("Table: "+table_name)
          println("Column: "+column_name)
          println("Raw value: "+rv)
          println()
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
              //INSERT IN USER REQUESTED CHOICE
              DbHandler.insert_expert_preference(expert_preference_type(default_values.int,table_name, column_name, rv, source, code))
              DbHandler.set_resolved(rv,table_name,column_name)
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
              println("Please input manually ontology and code")
              val user_choice = input_source_code()
              val source = user_choice._1
              val code = user_choice._2
              //INSERT IN USER REQUESTED CHOICE
              DbHandler.insert_expert_preference(expert_preference_type(default_values.int,table_name, column_name, rv, source, code))
              DbHandler.set_resolved(rv,table_name,column_name)
            }
            else if (user_choice.equals("2")){
              println("Please select an ID")
              val user_selection = get_choice(options.length)
              val a = options(user_selection.toInt-1)
              DbHandler.insert_expert_preference(expert_preference_type(default_values.int,a.table,a.column,a.raw_value,a.source.get,a.code.get))
              DbHandler.set_resolved(rv,table_name,column_name)
            }
            else if (user_choice.equals("3")){
              breakable {
                break()
              }
            }
            else if (user_choice.equals("4")){
              println("Returning to home page")
              println
              println
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
}

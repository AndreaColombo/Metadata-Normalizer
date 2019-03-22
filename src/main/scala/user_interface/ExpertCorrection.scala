package user_interface

import enricher.dbcon.{DbHandler, default_values, expert_preference_type}
import Utils._
import enricher.engine.RawValue

import scala.io.StdIn

object ExpertCorrection {

  def correct_value(value_to_correct: String,table: String, column: String): Unit = {
    println("Correct or invalidate?")
    println("1 - Correct")
    println("2 - Invalidate")
    val choice = get_choice(2)
    if (choice ==  "1") {
      val source_code = input_source_code()
      val rv = RawValue(value_to_correct, table, column)
      DbHandler.update_gcm_tid(rv, None)
      DbHandler.insert_expert_preference(expert_preference_type(table, column, value_to_correct, source_code._1, source_code._2))
      DbHandler.delete_raw_annotation(rv)
    }
    else {
      val rv = RawValue(value_to_correct, table, column)
      DbHandler.update_gcm_tid(rv, None)
      DbHandler.delete_raw_annotation(rv)
    }
  }

  def delete_ontology(): Unit = {
    var flag = false
    while (flag){
      println("Insert ontology to delete")
      var onto = StdIn.readLine()
      println
      while (!DbHandler.onto_exist(onto)){
        println("Ontology not found")
        println("Please input a valid ontology")
        onto = StdIn.readLine()
        println
        DbHandler.delete_ontology(onto)
        println("Do you wish to delete another ontology?")
        println("1 - Yes")
        println("2 - No")
        val choice = get_choice(2)

        if(choice.equalsIgnoreCase("1")){
          flag = true
        }
        else if(choice.equalsIgnoreCase("2"))
          flag = false
      }
    }
  }

  def correction_routine(): Unit = {
    display_prompt()
    val user_selection = get_choice(2)
    var flag = user_selection == "1"
    while (flag) {
      val tmp = Utils.choose_table_column()
      val table = tmp._1
      val column = tmp._2
      println(table)
      println(column)

      println("Input value to update")
      var value = StdIn.readLine()
      println
      while (!DbHandler.value_exist(value,table,column)){
        println("Value not valid")
        val options = DbHandler.get_suggestions_raw(value,table,column)
        if (options.nonEmpty) {
          println("Select one from these options")
          for (i <- options.indices) {
            println(i + 1 + " - " + options(i))
          }
          value = options(Utils.get_choice(options.length).toInt - 1)
          println("value chosen: " + value)
        }
        else {
          println("No suggestions found")
          println("Please input another value")
          value = StdIn.readLine()
          println
        }
      }
      correct_value(value,table,column)

      println("Do you wish to update another value?")
      println("1 - Yes")
      println("2 - No")
      val choice = get_choice(2)

      if(choice.equalsIgnoreCase("1")){
        flag = true
      }
      else if(choice.equalsIgnoreCase("2"))
        flag = false
    }
  }

  def display_prompt(): Unit = {
    println("1 - Start correction")
    println("2 - Go back to home page")
  }
}
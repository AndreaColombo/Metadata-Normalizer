package user_interface

import enricher.dbcon.{DbHandler, default_values, expert_preference_type}
import Utils._

import scala.io.StdIn

object ExpertCorrection {

  def correct_value(value_to_correct: String,table: String, column: String): Unit = {
    val source_code = input_source_code()
    DbHandler.update_tid(value_to_correct,None,table,column)
    val tuples = DbHandler.get_value_info(value_to_correct,table,column)

    for ((table_name, column_name) <- tuples) {
      DbHandler.insert_user_changes(expert_preference_type(default_values.int,table_name, column_name, value_to_correct, source_code._1, source_code._2))
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
      while (DbHandler.get_value_info(value,table,column).isEmpty){
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
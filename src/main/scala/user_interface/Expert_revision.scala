package user_interface

import Enricher.DBCon.{db_handler, default_values, expert_preference_type}
import user_interface.Expert_preference.input_source_code

import scala.io.StdIn

object Expert_revision {

  def revision_routine(): Unit = {
    var flag = true
    while (flag) {
      //CHIEDERE TABLE E COLUMN
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
        db_handler.insert_user_changes(expert_preference_type(default_values.int,table_name, column_name, value, source_code._1, source_code._2))
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
}

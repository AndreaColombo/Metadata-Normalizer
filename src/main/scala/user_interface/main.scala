package user_interface

import Enricher.DBCon.db_handler
import org.slf4j.LoggerFactory

object main {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    var exit = false
    while (!exit) {
      display_prompt()
      val selection = Expert_preference.get_choice(4)
      if (selection.equals("1")) {
        Expert_preference.get_user_feedback()
      }
      else if (selection.equals("2")) {
        Expert_correction.correction_routine()
      }
      else if (selection.equals("3")) {
        //EXPERT FEEDBACK
      }
      else exit = true
    }
    db_handler.get_suggestions_raw("b-cell","biosample","disease")
  }

  def display_prompt(): Unit = {
    println("1 - Expert Preference")
    println("2 - Expert Correction")
    println("3 - Expert Feedback")
    println("4 - Exit")
    println()
    println()
  }
}

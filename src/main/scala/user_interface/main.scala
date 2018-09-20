package user_interface

import Enricher.Enrichment_engine.Ols_interface
import org.slf4j.LoggerFactory
import Utils._

object main {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    var exit = false
    while (!exit) {
      display_prompt()
      val selection = get_choice(4)
      if (selection.equals("1")) {
        Expert_preference.get_user_feedback()
      }
      else if (selection.equals("2")) {
        Expert_correction.correction_routine()
      }
      else if (selection.equals("3")) {
        Expert_feedback.get_user_rating()
      }
      else exit = true
    }
    Ols_interface.ols_get_info("EFO","EFO_0003746")
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

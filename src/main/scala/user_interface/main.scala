package user_interface

import enricher.engine.OlsInterface
import org.slf4j.LoggerFactory
import Utils._

object main {
  val logger = LoggerFactory.getLogger(this.getClass)

  def main(args: Array[String]): Unit = {
    var exit = false
    while (!exit) {
      display_prompt()
      val selection = get_choice(5)
      if (selection.equals("1")) {
        ExpertPreference.get_user_feedback()
      }
      else if (selection.equals("2")) {
        ExpertCorrection.correction_routine()
      }
      else if (selection.equals("3")) {
        ExpertFeedback.get_user_rating()
      }
      else if (selection.equals("4")){
        ExpertCorrection.delete_ontology()
      }
      else exit = true
    }
  }

  def display_prompt(): Unit = {
    println("1 - Expert Preference")
    println("2 - Expert Correction")
    println("3 - Expert Feedback")
    println("4 - Delete Ontology")
    println("5 - Exit")
    println()
    println()
  }
}

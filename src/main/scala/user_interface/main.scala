package user_interface

import Recommender.Ontologies.Ontology

object main {

  def main(args: Array[String]): Unit = {
    var exit = true
    while (!exit) {
      display_prompt()
      if (args.nonEmpty) {
        if (args(0).equals("1")) {
          Expert_preference.get_user_feedback()
        }
        else if (args(0).equals("2")) {
          Expert_revision.revision_routine()
        }
        else if (args(0).equals("3")) {
          //EXPERT FEEDBACK
        }
        else exit = true
      }
    }
    val ols = Ontology.apply("ols")
    val a = ols.input("Ewing's Sarcoma")
    a.foreach(println)
  }

  def display_prompt(): Unit = {
    println("1 - Expert Preference")
    println("2 - Expert Revision")
    println("3 - Expert Feedback")
    println("4 - Exit")
  }
}

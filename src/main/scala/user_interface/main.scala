package user_interface

object main {

  def main(args: Array[String]): Unit = {
    var exit = false
    while (!exit) {
      display_prompt()
      val selection = Expert_preference.get_choice(4)
      if (selection.equals("1")) {
        Expert_preference.get_user_feedback()
      }
      else if (selection.equals("2")) {
        Expert_revision.revision_routine()
      }
      else if (selection.equals("3")) {
        //EXPERT FEEDBACK
      }
      else exit = true
    }
  }

  def display_prompt(): Unit = {
    println("1 - Expert Preference")
    println("2 - Expert Revision")
    println("3 - Expert Feedback")
    println("4 - Exit")
    println()
    println()
  }
}

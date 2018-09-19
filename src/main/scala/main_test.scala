import Enricher.DBCon.db_handler

object main_test {

  def main(args: Array[String]): Unit = {
//    user_interface.Expert_feedback.get_user_rating()
//    val a: Option[Int] = None
//    println(a.isDefined)
    db_handler.update_tid("RNA-Seq",None,"experiment_type","technique")
  }
}

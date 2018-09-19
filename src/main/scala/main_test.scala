import Enricher.DBCon.db_handler

object main_test {

  def main(args: Array[String]): Unit = {
    db_handler.init()
    user_interface.Expert_feedback.get_user_rating()
//    db_handler.update_tid("RNA-Seq",None,"experiment_type","technique")
  }
}

import Enricher.DBCon.db_handler

object main_test {

  def main(args: Array[String]): Unit = {
    db_handler.get_info_for_feedback("experiment_type","technique").foreach(println)
  }
}

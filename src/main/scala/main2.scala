import Config.config

object main2 {

  def main(args: Array[String]): Unit = {
    val t = args(0)
    val column_l = config.get_termtype_list(t)
    for (col <- column_l){
      println(col)
      db_filler.fill_db(t,col)
      db_filler.update_db(t,col)
    }
  }
}

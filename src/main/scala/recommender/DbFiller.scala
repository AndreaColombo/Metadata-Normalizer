package recommender

import java.util.Calendar

import ontologies.Ontology
import recommender.dbcon.DbHandler
import utilities.Preprocessing.parse

object DbFiller {

  val bioportal = Ontology.apply("bioportal")
  val recommender = Ontology.apply("recommender")
  val zooma = Ontology.apply("zooma")
  val ols = Ontology.apply("ols")

  def fill_db (table_name: String, column_name: String): Unit = {
    val s = parse(DbHandler.get_raw_values(table_name,column_name)).filterNot(_.isEmpty).mkString(",")
//    val c = s.split(",").filterNot(sd => sd.equalsIgnoreCase("p12")).mkString(",")
//    val b = c.split(",").filterNot(sd => sd.equalsIgnoreCase("h54")).mkString(",")
    println(column_name)
    get_timestamp()
//    println(s)
    var split = false
    var recsys1 = ""
    var recsys2 = ""
    var recsys3 = ""
    var recsys4 = ""
    if (s.split(",").length>150) {
      split = true
      val tmp = s.split(",")
      val tmp1 = tmp.splitAt(tmp.length / 2)._1.toList
      val tmp2 = tmp.splitAt(tmp.length / 2)._2.toList
      recsys1 = tmp1.splitAt(tmp1.length / 2)._1.mkString(",")
      recsys2 = tmp1.splitAt(tmp1.length / 2)._2.mkString(",")
      recsys3 = tmp2.splitAt(tmp2.length / 2)._1.mkString(",")
      recsys4 = tmp2.splitAt(tmp2.length / 2)._2.mkString(",")
    }

    println("bioportal inizio")
    DbHandler.apiresults_insert(bioportal.input(s),column_name)
    println("bioportal fine")
    get_timestamp()

    if(split) {
      println("recsys 1 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys1), column_name)
      println("recsys 1 fine")
      get_timestamp()

      println("recsys 2 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys2), column_name)
      println("recsys 2 fine")
      get_timestamp()

      println("recsys 3 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys3), column_name)
      println("recsys 3 fine")
      get_timestamp()

      println("recsys 4 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys4), column_name)
      println("recsys 4 fine")
      get_timestamp()
    }
    else {
      println("recsys inizio")
      val aa = recommender.input(s)
      DbHandler.apiresults_insert(aa,column_name)
      println("recsys fine")
    }
    println("zooma inizio")
    DbHandler.apiresults_insert(zooma.input(s),column_name)
    println("zooma fine")
    get_timestamp()

    println("ols inizio")
    DbHandler.apiresults_insert(ols.input(s),column_name)
    println("ols fine")
    get_timestamp()
  }

  def get_timestamp(): Unit = {
    val now = Calendar.getInstance()
    println(now.get(Calendar.HOUR_OF_DAY)+":"+now.get(Calendar.MINUTE)+":"+now.get(Calendar.SECOND))
  }
}

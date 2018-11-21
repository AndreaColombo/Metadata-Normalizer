package recommender

import java.util.Calendar

import ontologies.Ontology
import recommender.dbcon.DbHandler
import utilities.Preprocessing.parse

/**
  * This object contains the method used to fill apiresults table
  */
object DbFiller {

  val bioportal = Ontology.apply("bioportal")
  val recommender = Ontology.apply("recommender")
  val zooma = Ontology.apply("zooma")
  val ols = Ontology.apply("ols")

  /**
    * Fills in apiresults table with datas from the various services
    * First takes the raw values from the GCM, parse them and calls the various services to obtain datas, finally insert the datas in the database
    * @param table_name Table of GCM from where to take the raw values
    * @param column_name Column of the GCM
    */
  def fill_db (table_name: String, column_name: String): Unit = {
    val s = parse(DbHandler.get_raw_values(table_name,column_name)).filterNot(_.isEmpty).mkString(",")
    println(column_name)
    utilities.Utils.get_timestamp()
    var recsys1 = ""
    var recsys2 = ""
    var recsys3 = ""
    var recsys4 = ""
    val is_split = s.split(",").length > 150
    if (is_split) {
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
    utilities.Utils.get_timestamp()

    if(is_split) {
      println("recsys 1 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys1), column_name)
      println("recsys 1 fine")
      utilities.Utils.get_timestamp()

      println("recsys 2 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys2), column_name)
      println("recsys 2 fine")
      utilities.Utils.get_timestamp()

      println("recsys 3 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys3), column_name)
      println("recsys 3 fine")
      utilities.Utils.get_timestamp()

      println("recsys 4 inizio")
      DbHandler.apiresults_insert(recommender.input(recsys4), column_name)
      println("recsys 4 fine")
      utilities.Utils.get_timestamp()
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
    utilities.Utils.get_timestamp()

    println("ols inizio")
    DbHandler.apiresults_insert(ols.input(s),column_name)
    println("ols fine")
    utilities.Utils.get_timestamp()
  }
}

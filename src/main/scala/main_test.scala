import java.net.HttpCookie

import config_pkg.ApplicationConfig
import config_pkg.ApplicationConfig._
import enricher.dbcon.DbHandler
import enricher.engine.OlsInterface._
import enricher.engine.{OlsInterface, RawValue, ScoredTerm, Term}
import recommender.OntologiesSetCalculator
import scalaj.http.{Http, HttpOptions}
import utilities.Utils

import sys.process._

object main_test {

  def main(args: Array[String]): Unit = {

    val term = Term(ols_get_onto_info("doid"),"DOID_707",ols_get_iri("doid","DOID_707"))
    try {
      term.fill().fill_relation()
    }
    catch {
      case e: Exception => logger.info(e.toString)
    }

//    DbHandler.insert_ontology(ols_get_onto_info("doid"))
    //    val table_l = ApplicationConfig.get_gcm_table_list()
//    for (t <- table_l) {
//      recommender.ReportWriter.write_report(t)
//    }
  }
}


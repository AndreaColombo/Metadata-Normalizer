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


    val a = Term(ols_get_onto_info("ncit"),"NCIT_C8863","http://purl.obolibrary.org/obo/NCIT_C8863")

    a.fill().fill_relation().saveToKB().save_relation()

    ApplicationConfig.get_termtype_list("experiment_type").foreach(println)

  }
}


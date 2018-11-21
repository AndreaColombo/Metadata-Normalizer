import java.net.HttpCookie

import config_pkg.ApplicationConfig._
import enricher.engine.OlsInterface._
import enricher.engine.{OlsInterface, RawValue, ScoredTerm, Term}
import scalaj.http.{Http, HttpOptions}
import utilities.Utils

import sys.process._

object main_test {

  def main(args: Array[String]): Unit = {
    //    val term = Term(OlsInterface.ols_get_onto_info("uberon"),"","http://purl.obolibrary.org/obo/UBERON_0002107")

    //    val tmp = term.fill().saveToKB().fill_relation()
    //    tmp.save_relation()
  }
}


import java.net.HttpCookie

import config_pkg.ApplicationConfig
import enricher.engine.Term
import enricher.engine.OlsInterface._

object main_test {

  def main(args: Array[String]): Unit = {

    val a = ols_get_info("UBERON","UBERON_0001295",ols_get_iri("uberon","UBERON_0001295"))
    a.synonyms.get.foreach(println)

  }
}


import java.net.HttpCookie

import config_pkg.ApplicationConfig
import enricher.engine.Term
import enricher.engine.OlsInterface._

object main_test {

  def main(args: Array[String]): Unit = {


   print(ols_exist("NCBITaxon",ols_get_iri("NCBITaxon","NCBITaxon_9606")))

  }
}


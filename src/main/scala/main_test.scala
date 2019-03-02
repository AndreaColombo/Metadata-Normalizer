import java.net.HttpCookie

import config_pkg.ApplicationConfig

object main_test {

  def main(args: Array[String]): Unit = {


//    val a = Term(ols_get_onto_info("ncit"),"NCIT_C8863","http://purl.obolibrary.org/obo/NCIT_C8863")
//
//    a.fill().fill_relation().saveToKB().save_relation()
    val dummy = ""
    ApplicationConfig.get_termtype_list("experiment_type").foreach(println)

  }
}


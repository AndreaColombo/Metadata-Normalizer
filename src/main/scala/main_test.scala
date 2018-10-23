import config_pkg.ApplicationConfig._
import enricher.engine.Ols_interface._
import enricher.engine.{Ols_interface, RawValue, ScoredTerm, Term}
import scalaj.http.{Http, HttpOptions}

object main_test {

  def main(args: Array[String]): Unit = {
    val term = Term(Ols_interface.ols_get_onto_info("uberon"),"","http://purl.obolibrary.org/obo/UBERON_0002107")

//    val tmp = term.fill().saveToKB().fill_relation()
//    tmp.save_relation()

    val response = Http("https://www.ebi.ac.uk/ols/api/ontologies/uberon/terms/http%253A%252F%252Fpurl.obolibrary.org%252Fobo%252FUBERON_0002107").option(HttpOptions.readTimeout(50000)).asString
    println(response.is2xx)

    //.parents.get.foreach(a => println(a.term.left.get))
  }
}

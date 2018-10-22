import config_pkg.ApplicationConfig._
import enricher.engine.Ols_interface._
import enricher.engine.{Ols_interface, RawValue, ScoredTerm, Term}

object main_test {

  def main(args: Array[String]): Unit = {
    val term = Term(Ols_interface.ols_get_onto_info("doid"),"DOID_707",ols_get_iri("doid","DOID_707"))

    println(term.fill().toString)
  }
}

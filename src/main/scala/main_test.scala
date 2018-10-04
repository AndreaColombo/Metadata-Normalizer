import config_pkg.ApplicationConfig._
import enricher.engine.Ols_interface._
import enricher.engine.{RawValue, ScoredTerm, Term}

object main_test {

  def main(args: Array[String]): Unit = {

    val threshold = get_threshold()
//    val rv = RawValue("b cell lymphoma","biosample","disease")
    val rv = RawValue("liver","biosample","tissue")

  }
}

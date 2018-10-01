import java.net.URLEncoder

import enricher.engine.Ols_interface._
import enricher.engine.{RawValue, Term}

object main_test {

  def main(args: Array[String]): Unit = {

    val terms = ols_search_term(RawValue("b cell lymphoma","biosample","disease")).map(_.fill).foreach(println)


  }
}

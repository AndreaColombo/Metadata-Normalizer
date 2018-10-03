import java.net.URLEncoder

import config_pkg.ApplicationConfig
import enricher.engine.Ols_interface._
import enricher.engine.{RawValue, ScoredTerm, Term}

object main_test {

  def main(args: Array[String]): Unit = {

    val threshold = ApplicationConfig.get_threshold()
    val rv = RawValue("b cell lymphoma","biosample","disease")
    val terms = ols_search_term(rv).map(_.fill).map(a => ScoredTerm(a,get_score("b cell lymphoma",a.prefLabel.get,a.synonyms.get)))
      .filterNot(_.score < threshold).sortWith(_.score>_.score).head.term.copy(rawValue = Some(rv)).saveToKB()

    println(terms)

//    ols_get_info("uberon",ols_get_iri("uberon","UBERON_0001280"))

  }
}

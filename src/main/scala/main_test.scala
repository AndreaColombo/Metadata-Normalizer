import config_pkg.ApplicationConfig._
import enricher.engine.Ols_interface._
import enricher.engine.{RawValue, ScoredTerm, Term}

object main_test {

  def main(args: Array[String]): Unit = {

    val threshold = get_threshold()
//    val rv = RawValue("b cell lymphoma","biosample","disease")
    val rv = RawValue("liver","biosample","tissue")

    val terms = ols_search_term(rv).map(_.fill()).map(_.copy(rawValue = Some(rv))).map(a => ScoredTerm(a,get_score(a.rawValue.get.value,a.prefLabel.get,a.synonyms.get))).sortWith(_.score >_.score)


    val terms_filtered = terms.filterNot(_.score < threshold)

    val best_term = terms_filtered.head
    val matchModeRandom = true//get_search_mode()
    if(!matchModeRandom){
      val best_terms = terms_filtered.filter(_.score == best_term.score)
      if(best_terms.nonEmpty){
//        insert in user fb best terms
      }
      else best_term.term.saveToKB()
    }
    else best_term.term.saveToKB()

    println(best_term.term)
  }
}

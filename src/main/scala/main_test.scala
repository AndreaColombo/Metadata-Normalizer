import Config.config
import Enricher.DBCon.db_handler
import Recommender.Ontologies.Parsers.OlsParser
import Utilities.score_calculator

object main_test {

  def main(args: Array[String]): Unit = {
    val term = "c b"
    val label = "b c e"
    val diff = score_calculator.convert_score_num(OlsParser.get_score(term,label),"ols")


    println(diff)

  }
}

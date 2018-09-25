import config.Config
import enricher.dbcon.DbHandler
import enricher.engine.Ols_interface
import recommender.ontologies.Parsers.OlsParser
import utilities.ScoreCalculator

object main_test {

  def main(args: Array[String]): Unit = {
    val term = "c b"
    val label = "b c e"
    println(Ols_interface.ols_get_info("ncbitaxon","NCBITaxon_9606").head(4))
  }
}

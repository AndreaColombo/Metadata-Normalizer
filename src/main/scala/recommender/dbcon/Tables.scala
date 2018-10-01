package recommender.dbcon

import slick.jdbc.PostgresProfile.api._

object Tables {

  class OntologyScore (tag: Tag) extends Table [(String, Double)](tag, Some("public"), "ontologyscore"){
    def ontology = column[String]("ontology")
    def score = column[Double]("score")
    def * = (ontology,score)
  }
  val ontologyScore = TableQuery[OntologyScore]

  class best_ontos (tag: Tag) extends Table [(String, String, Double, Double, Double)](tag, Some("public"), "best_onto_sets"){
    def term_type = column[String]("term_type")
    def ontology_set = column[String]("ontologies_set")
    def coverage = column[Double]("set_coverage")
    def score = column[Double]("set_score1")
    def suitability = column[Double]("set_suitability")
    def * = (term_type,ontology_set,coverage,score,suitability)
  }
  val best_onto_set = TableQuery[best_ontos]

  class ApiResults2(tag: Tag) extends Table[(Int, String, String, String, String, String, String, String, String, String, Double, Double, Double, Double, Double, Boolean)](tag, Some("public"), "apiresults"){
    def id = column[Int]("id", O.AutoInc)
    def service = column[String]("service")
    def raw_value = column[String]("raw_value")
    def parsed_value = column[String]("parsed_value")
    def ontology = column[String]("ontology")
    def ontology_id = column[String]("ontology_id")
    def pref_label = column[String]("iri")
    def synonym = column[String]("synonym")
    def score = column[String]("score")
    def term_type = column[String]("term_type")
    def match_score = column[Double]("match_score",O.Default(0.0))
    def onto_score = column[Double]("onto_score",O.Default(0.0))
    def score_num1 = column[Double]("score_num1",O.Default(0.0))
    def score_num2 = column[Double]("score_num2",O.Default(0.0))
    def suitability = column[Double]("suitability",O.Default(0.0))
    def ok = column[Boolean]("ok",O.Default(false))
    def * = (id,service,term_type,raw_value,parsed_value,ontology,ontology_id,pref_label,synonym,score,match_score,onto_score,score_num1,score_num2,suitability,ok)
  }
  val apiresults = TableQuery[ApiResults2]
}

package DBcon

import slick.jdbc.PostgresProfile.api._

object Tables {

  class t1(tag: Tag) extends Table[(String, String, String)](tag, Some("svr"),"t1") {
    def s_id = column[String]("s_id", O.PrimaryKey)
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (s_id, key, value)
  }
  val t1 = TableQuery[t1]

  class t2(tag: Tag) extends Table[(String, String, String)](tag, "t2") {
    def s_id = column[String]("s_id")
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (s_id, key, value)
  }

  class prova(tag: Tag) extends Table[(String, String, String)](tag, Some("svr"), "prova") {
    def s_id = column[String]("s_id")
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (s_id, key, value)
 }

  class ApiResults(tag: Tag) extends Table[(String, String, String, String, String, String, String, String, String)](tag, Some("svr"), "apiresults1"){
    def service = column[String]("service")
    def raw_value = column[String]("raw_value")
    def parsed_value = column[String]("parsed_value")
    def ontology = column[String]("ontology")
    def ontology_id = column[String]("ontology_id")
    def pref_label = column[String]("pref_label")
    def synonym = column[String]("synonym")
    def score = column[String]("score")
    def term_type = column[String]("term_type")
    def id = column[Int]("id")
    def deleted = column[Boolean]("deleted")
    def * = (service,raw_value,parsed_value,ontology,ontology_id,pref_label,synonym,score,term_type)
  }

  class OntologyScore (tag: Tag) extends Table [(String, String, String)](tag, Some("svr"), "ontologyscore"){
    def ontology = column[String]("ontology")
    def term_type = column[String]("term_type")
    def score = column[String]("score")
    def * = (ontology,term_type,score)
  }

  class best_ontos (tag: Tag) extends Table [(String, String, Double, Double, Double)](tag, Some("svr"), "best_ontos"){
    def term_type = column[String]("term_type")
    def ontology_set = column[String]("ontologies set")
    def coverage = column[Double]("set_coverage")
    def score = column[Double]("set_score")
    def suitability = column[Double]("set_suitability")
    def * = (term_type,ontology_set,coverage,score,suitability)
  }

  class ApiResults2(tag: Tag) extends Table[(String, String, String, String, String, String, String, String, String)](tag, Some("svr"), "apiresults2"){
    def service = column[String]("service")
    def raw_value = column[String]("raw_value")
    def parsed_value = column[String]("parsed_value")
    def ontology = column[String]("ontology")
    def ontology_id = column[String]("ontology_id")
    def pref_label = column[String]("pref_label")
    def synonym = column[String]("synonym")
    def score = column[String]("score")
    def term_type = column[String]("term_type")
    def id = column[Int]("id")
    def * = (service,raw_value,parsed_value,ontology,ontology_id,pref_label,synonym,score,term_type)
  }
}

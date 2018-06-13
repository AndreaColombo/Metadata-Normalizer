package DBcon

import javafx.css.ParsedValue
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

  class OntologyScore (tag: Tag) extends Table [(String, Double)](tag, Some("svr"), "ontologyscore"){
    def ontology = column[String]("ontology")
    def score = column[Double]("score")
    def * = (ontology,score)
  }

  class best_ontos (tag: Tag) extends Table [(String, String, Double, Double, Double, Double)](tag, Some("svr"), "best_ontos2"){
    def term_type = column[String]("term_type")
    def ontology_set = column[String]("ontologies_set")
    def coverage = column[Double]("set_coverage")
    def score = column[Double]("set_score1")
    def score2 = column[Double]("set_score2")
    def suitability = column[Double]("set_suitability")
    def * = (term_type,ontology_set,coverage,score,score2,suitability)
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

  class cv_support(tag: Tag) extends Table[(String,String,String)](tag, Some("public"), "cv_support"){
    def tid = column[Int]("tid")
    def source = column[String]("source")
    def code = column[String]("code")
    def label = column[String]("pref_label")
    def * = (source,code,label)
  }

  class cv_support_syn(tag: Tag) extends Table[(Int,String,String)](tag,Some("public"), "cv_support_syn"){
    def tid = column[Int]("tid")
    def label = column[String]("label")
    def ttype = column[String]("type")
    def * = (tid, label,ttype)
  }

  class cv_support_xref(tag: Tag) extends Table[(Int,String,String)](tag, Some("public"), "cv_support_xref"){
    def tid = column[Int]("tid")
    def source = column[String]("source")
    def code = column[String]("code")
    def * = (tid, source, code)
  }

  class onto_support_hyp(tag: Tag) extends Table[(Int,Int,String)](tag, Some("public"), "onto_support_hyp"){
    def tid_p = column[Int]("tid_parent")
    def tid_c = column[Int]("tid_child")
    def ttype = column[String]("type")
    def * = (tid_p, tid_c, ttype)
  }

  class user_feedback(tag: Tag) extends Table[(String, String, String, String)](tag, "user_feedback"){
    def raw_value = column[String]("raw_value")
    def parsed_value = column[String]("parsed_value")
    def label = column[String]("label")
    def id = column[String]("id")
    def * = (raw_value, parsed_value, label, id)
  }

}

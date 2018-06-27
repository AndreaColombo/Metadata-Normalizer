package DBcon

import javafx.css.ParsedValue
import slick.ast.ColumnOption.Unique
import slick.jdbc.PostgresProfile.api._

object Tables {

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

  class cv_support(tag: Tag) extends Table[(Int, String,String,String)](tag, Some("public"), "cv_support"){
    def tid = column[Int]("tid", O.AutoInc, O.PrimaryKey)
    def source = column[String]("source")
    def code = column[String]("code")
    def label = column[String]("pref_label")

    def idx = index("cv_support_source_code_key",(source,code),unique = true)

    def * = (tid, source,code,label)
  }
  val cv_support = TableQuery[cv_support]

  class cv_support_syn(tag: Tag) extends Table[(Int,String,String)](tag,Some("public"), "cv_support_syn"){
    def tid = column[Int]("tid")
    def label = column[String]("label")
    def ttype = column[String]("type")

    def pk = primaryKey("cv_support_syn_pkey", (tid,label,ttype))
    def fk = foreignKey("cv_cvs_fk",tid, cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def idx = index("syn_label_idx", label)

    def * = (tid, label,ttype)
  }
  val cv_support_syn = TableQuery[cv_support_syn]

  class cv_support_xref(tag: Tag) extends Table[(Int,String,String)](tag, Some("public"), "cv_support_xref"){
    def tid = column[Int]("tid")
    def source = column[String]("source")
    def code = column[String]("code")

    def pk = primaryKey("cv_support_xref_pley", (tid, source, code))
    def fk = foreignKey("cv_cvx_fk",tid, cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def * = (tid, source, code)
  }
  val cv_support_xref = TableQuery[cv_support_xref]

  class onto_support_hyp(tag: Tag) extends Table[(Int,Int,String)](tag, Some("public"), "onto_support_hyp"){
    def tid_p = column[Int]("tid_parent")
    def tid_c = column[Int]("tid_child")
    def ttype = column[String]("type")

    def pk = primaryKey("onto_support_hyp_pkey",(tid_p,tid_c,ttype))
    def fk_p = foreignKey("cv_ohp_fk",tid_p,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_c = foreignKey("cv_ohc_fk",tid_c,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def * = (tid_p, tid_c, ttype)
  }
  val onto_support_hyp = TableQuery[onto_support_hyp]

  class user_feedback(tag: Tag) extends Table[(Int, Boolean, String, String, String, Option[String], Option[String], Option[String], Option[String])](tag, "user_feedback"){
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def raw_value = column[String]("raw_value")
    def parsed_value = column[Option[String]]("parsed_value")
    def label = column[Option[String]]("label")
    def source = column[Option[String]]("source")
    def code = column[Option[String]]("code")
    def resolved = column[Boolean]("resolved", O.Default(false))
    def * = (id, resolved, table_name, column_name, raw_value, parsed_value, label, source, code)
  }
  val user_feedback = TableQuery[user_feedback]

  class user_changes(tag: Tag) extends Table[(Int, String, String, String, String, String)](tag, "user_requested_changes"){
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def raw_value = column[String]("raw_value")
    def source = column[String]("source")
    def code = column[String]("code")
    def * = (id, table_name, column_name, raw_value, source, code)
  }
  val user_changes = TableQuery[user_changes]

}
package DBcon

import javafx.css.ParsedValue
import slick.ast.ColumnOption.Unique
import slick.jdbc.PostgresProfile.api._
import slick.sql.SqlProfile.ColumnOption.SqlType

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

  class cv_support(tag: Tag) extends Table[(Int, String,String,String, String)](tag, Some("public"), "cv_support"){
    def tid = column[Int]("tid", O.AutoInc, O.PrimaryKey)
    def source = column[String]("source", O.SqlType("VARCHAR(8)"))
    def code = column[String]("code",O.SqlType("VARCHAR(16)"))
    def label = column[String]("pref_label", O.SqlType("VARCHAR(128)"))
    def description = column[String]("description")

    def idx = index("cv_support_source_code_key",(source,code),unique = true)

    def * = (tid, source,code,label,description)
  }
  val cv_support = TableQuery[cv_support]

  class cv_support_syn(tag: Tag) extends Table[(Int,String,String)](tag,Some("public"), "cv_support_syn"){
    def tid = column[Int]("tid")
    def label = column[String]("label", O.SqlType("VARCHAR(128)"))
    def ttype = column[String]("type", O.SqlType("VARCHAR(4)"))

    def pk = primaryKey("cv_support_syn_pkey", (tid,label,ttype))
    def fk = foreignKey("cv_cvs_fk",tid, cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def idx = index("syn_label_idx", label)

    def * = (tid, label,ttype)
  }
  val cv_support_syn = TableQuery[cv_support_syn]

  class cv_support_xref(tag: Tag) extends Table[(Int,String,String)](tag, Some("public"), "cv_support_xref"){
    def tid = column[Int]("tid")
    def source = column[String]("source", O.SqlType("VARCHAR(8)"))
    def code = column[String]("code",O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("cv_support_xref_pley", (tid, source, code))
    def fk = foreignKey("cv_cvx_fk",tid, cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("xref_code_idx",code)

    def * = (tid, source, code)
  }
  val cv_support_xref = TableQuery[cv_support_xref]

  class cv_support_raw(tag: Tag) extends Table[(Int, String, String, String, Char)](tag, Some("public"),"cv_support_raw"){
    def tid = column[Int]("tid")
    def label = column[String]("label", O.SqlType("VARCHAR(128)"))
    def table_name = column[String]("table_name", O.SqlType("VARCHAR(16)"))
    def column_name = column[String]("column_name", O.SqlType("VARCHAR(16)"))
    def method = column[Char]("method")

    def pk = primaryKey("cv_support_raw_pkey",(label,table_name,column_name))
    def fk = foreignKey("cv_cvr_fk",tid,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def * = (tid,label,table_name,column_name,method)
  }
  val cv_support_raw = TableQuery[cv_support_raw]

  class onto_support_hyp(tag: Tag) extends Table[(Int,Int,String)](tag, Some("public"), "onto_support_hyp"){
    def tid_p = column[Int]("tid_parent")
    def tid_c = column[Int]("tid_child")
    def rel_type = column[String]("rel_type", O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("onto_support_hyp_pkey",(tid_p,tid_c,rel_type))
    def fk_p = foreignKey("cv_ohp_fk",tid_p,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_c = foreignKey("cv_ohc_fk",tid_c,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("ohc_idx",tid_c)

    def * = (tid_p, tid_c, rel_type)
  }
  val onto_support_hyp = TableQuery[onto_support_hyp]

  class onto_support_hyp_unfolded(tag: Tag) extends  Table[(Int,Int,Int,String)](tag, Some("public"),"onto_support_hyp_unfolded"){
    def tid_a = column[Int]("tid_ancestor")
    def tid_d = column[Int]("tid_descendant")
    def distance = column[Int]("distance")
    def rel_type = column[String]("rel_type", O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("onto_support_hyp_unfolded_pk",(tid_a,tid_d,rel_type))
    def fk_a = foreignKey("cv_oha_unfolded",tid_a,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_d = foreignKey("cv_ohd_unfolded",tid_d,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("ohd_unfolded_idx", tid_d)

    def * = (tid_a,tid_d,distance,rel_type)
  }

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

    def idx = index("user_feedback_idx",(raw_value,source,code),unique = true)

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
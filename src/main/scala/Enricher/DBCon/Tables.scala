package Enricher.DBCon

import slick.jdbc.PostgresProfile.api._

object default_values{
  def int: Int = -1
  def string = "null"
  def char = 'n'
  def bool = false
}

case class cv_support_type(tid: Int = default_values.int, source: String = default_values.string, code: String = default_values.string, label: String = default_values.string, description: String = default_values.string)
case class cv_support_syn_type (tid: Int = default_values.int, label: String = default_values.string, ttype: String = default_values.string)
case class cv_support_xref_type (tid: Int = default_values.int, source: String = default_values.string, code: String = default_values.string)
case class cv_support_raw_type(tid: Int = default_values.int, label: String = default_values.string, table_name: String = default_values.string, column_name: String = default_values.string, method: Char = default_values.char)
case class user_feedback_type (id: Int = default_values.int, resolved: Boolean = default_values.bool, table: String = default_values.string, column: String = default_values.string, tid: Option[Int] = Some(default_values.int), raw_value: String = default_values.string, parsed_value: Option[String] = Some(default_values.string), label: Option[String] = Some(default_values.string), source: Option[String] = Some(default_values.string), code: Option[String] = Some(default_values.string))
case class user_changes_type (id: Int = default_values.int, table_name: String = default_values.string, column_name: String = default_values.string, raw_value: String = default_values.string, source: String = default_values.string, code: String = default_values.string)
case class ontology_type(source: String = default_values.string, title: Option[String] = Some(default_values.string), description: Option[String] = Some(default_values.string), url: Option[String] = Some(default_values.string))
case class onto_support_hyp_type (tid_p: Int = default_values.int, tid_c: Int = default_values.int, rel_type:String = default_values.string)
case class onto_support_hyp_unfolded_type (tid_a: Int = default_values.int , tid_d: Int = default_values.int, distance: Int = default_values.int, rel_type:String = default_values.string)

object Tables {

  class ontology (tag: Tag) extends Table[ontology_type](tag, Some("public"), "ontology"){
    def source = column[String]("source",O.SqlType("VARCHAR(64)"), O.PrimaryKey)
    def title = column[Option[String]]("title",O.SqlType("VARCHAR(64)"))
    def description = column[Option[String]]("description")
    def url = column[Option[String]]("url",O.SqlType("VARCHAR(128)"))

    def * = (source,title,description,url) <> (ontology_type.tupled, ontology_type.unapply)
  }
  val ontology = TableQuery[ontology]

  class cv_support(tag: Tag) extends Table[cv_support_type](tag, Some("public"), "cv_support"){
    def tid = column[Int]("tid", O.AutoInc, O.PrimaryKey)
    def source = column[String]("source", O.SqlType("VARCHAR(32)"))
    def code = column[String]("code",O.SqlType("VARCHAR(64)"))
    def label = column[String]("pref_label", O.SqlType("VARCHAR(128)"))
    def description = column[String]("description")

    def idx = index("cv_support_source_code_key",(source,code),unique = true)
    def fk = foreignKey("ontology_source_fk",source,ontology)(_.source, onDelete = ForeignKeyAction.Cascade)

    def * = (tid, source,code,label,description) <> (cv_support_type.tupled, cv_support_type.unapply)
  }
  val cv_support = TableQuery[cv_support]

  class cv_support_syn(tag: Tag) extends Table[cv_support_syn_type](tag,Some("public"), "cv_support_syn"){
    def tid = column[Int]("tid")
    def label = column[String]("label", O.SqlType("VARCHAR(128)"))
    def ttype = column[String]("type", O.SqlType("VARCHAR(4)"))

    def pk = primaryKey("cv_support_syn_pkey", (tid,label,ttype))
    def fk = foreignKey("cv_cvs_fk",tid, cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def idx = index("syn_label_idx", label)

    def * = (tid, label,ttype) <> (cv_support_syn_type.tupled, cv_support_syn_type.unapply)
  }
  val cv_support_syn = TableQuery[cv_support_syn]

  class cv_support_xref(tag: Tag) extends Table[cv_support_xref_type](tag, Some("public"), "cv_support_xref"){
    def tid = column[Int]("tid")
    def source = column[String]("source", O.SqlType("VARCHAR(128)"))
    def code = column[String]("code")

    def pk = primaryKey("cv_support_xref_pley", (tid, source, code))
    def fk = foreignKey("cv_cvx_fk",tid, cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("xref_code_idx",code)

    def * = (tid, source, code) <> (cv_support_xref_type.tupled, cv_support_xref_type.unapply)
  }
  val cv_support_xref = TableQuery[cv_support_xref]

  class cv_support_raw(tag: Tag) extends Table[cv_support_raw_type](tag, Some("public"),"cv_support_raw"){
    def tid = column[Int]("tid")
    def label = column[String]("label", O.SqlType("VARCHAR(128)"))
    def table_name = column[String]("table_name", O.SqlType("VARCHAR(32)"))
    def column_name = column[String]("column_name", O.SqlType("VARCHAR(32)"))
    def method = column[Char]("method")

    def pk = primaryKey("cv_support_raw_pkey",(label,table_name,column_name))
    def fk = foreignKey("cv_cvr_fk",tid,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def * = (tid,label,table_name,column_name,method) <> (cv_support_raw_type.tupled, cv_support_raw_type.unapply)
  }
  val cv_support_raw = TableQuery[cv_support_raw]


  class onto_support_hyp(tag: Tag) extends Table[onto_support_hyp_type](tag, Some("public"), "onto_support_hyp"){
    def tid_p = column[Int]("tid_parent")
    def tid_c = column[Int]("tid_child")
    def rel_type = column[String]("rel_type", O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("onto_support_hyp_pkey",(tid_p,tid_c,rel_type))
    def fk_p = foreignKey("cv_ohp_fk",tid_p,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_c = foreignKey("cv_ohc_fk",tid_c,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("ohc_idx",tid_c)

    def * = (tid_p, tid_c, rel_type) <> (onto_support_hyp_type.tupled, onto_support_hyp_type.unapply)
  }
  val onto_support_hyp = TableQuery[onto_support_hyp]

  class onto_support_hyp_unfolded(tag: Tag) extends  Table[onto_support_hyp_unfolded_type](tag, Some("public"),"onto_support_hyp_unfolded"){
    def tid_a = column[Int]("tid_ancestor")
    def tid_d = column[Int]("tid_descendant")
    def distance = column[Int]("distance")
    def rel_type = column[String]("rel_type", O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("onto_support_hyp_unfolded_pk",(tid_a,tid_d,rel_type))
    def fk_a = foreignKey("cv_oha_unfolded",tid_a,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_d = foreignKey("cv_ohd_unfolded",tid_d,cv_support)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("ohd_unfolded_idx", tid_d)

    def * = (tid_a,tid_d,distance,rel_type) <> (onto_support_hyp_unfolded_type.tupled, onto_support_hyp_unfolded_type.unapply)
  }
  val onto_support_hyp_unfolded = TableQuery[onto_support_hyp_unfolded]

  class user_feedback(tag: Tag) extends Table[user_feedback_type](tag, "user_feedback"){
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def raw_value = column[String]("raw_value")
    def tid = column[Option[Int]]("tid")
    def parsed_value = column[Option[String]]("parsed_value")
    def label = column[Option[String]]("label")
    def source = column[Option[String]]("source")
    def code = column[Option[String]]("code")
    def resolved = column[Boolean]("resolved", O.Default(false))

    def idx = index("user_feedback_idx",(tid,raw_value,source,code),unique = true)

    def * = (id, resolved, table_name, column_name, tid, raw_value, parsed_value, label, source, code) <> (user_feedback_type.tupled, user_feedback_type.unapply)
  }
  val user_feedback = TableQuery[user_feedback]

  class user_changes(tag: Tag) extends Table[user_changes_type](tag, "user_requested_changes"){
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def raw_value = column[String]("raw_value")
    def source = column[String]("source")
    def code = column[String]("code")
    def * = (id, table_name, column_name, raw_value, source, code) <> (user_changes_type.tupled,user_changes_type.unapply)
  }
  val user_changes = TableQuery[user_changes]



}
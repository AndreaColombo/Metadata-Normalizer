package Enricher.DBCon

import slick.jdbc.PostgresProfile.api._

object default_values{
  def int: Int = -1
  def string = "null"
  def char = 'n'
  def bool = false
}

case class ontology_type(source: String = default_values.string, title: Option[String] = Some(default_values.string), description: Option[String] = Some(default_values.string), url: Option[String] = Some(default_values.string))
case class vocabulary_type(tid: Int = default_values.int, source: String = default_values.string, code: String = default_values.string, label: String = default_values.string, description: String = default_values.string, iri: String = default_values.string)
case class synonym_type(tid: Int = default_values.int, label: String = default_values.string, ttype: String = default_values.string)
case class reference_type(tid: Int = default_values.int, source: String = default_values.string, code: String = default_values.string)
case class raw_annotation_type(tid: Int = default_values.int, label: String = default_values.string, table_name: String = default_values.string, column_name: String = default_values.string, method: Char = default_values.char)
case class relationship_type(tid_p: Int = default_values.int, tid_c: Int = default_values.int, rel_type:String = default_values.string)
case class relationship_unfolded_type(tid_a: Int = default_values.int, tid_d: Int = default_values.int, distance: Int = default_values.int, rel_type:String = default_values.string)

case class expert_choice_type(id: Int = default_values.int, resolved: Boolean = default_values.bool, table: String = default_values.string, column: String = default_values.string, tid: Option[Int] = Some(default_values.int), raw_value: String = default_values.string, parsed_value: Option[String] = Some(default_values.string), label: Option[String] = Some(default_values.string), source: Option[String] = Some(default_values.string), code: Option[String] = Some(default_values.string), iri: Option[String]= Some(default_values.string), provenance: String = default_values.string, timestamp: String = default_values.string)
case class expert_preference_type(id: Int = default_values.int, table_name: String = default_values.string, column_name: String = default_values.string, raw_value: String = default_values.string, source: String = default_values.string, code: String = default_values.string)
case class expert_feedback_type(expert_username: String = default_values.string, raw_value: String = default_values.string, table_name: String = default_values.string, column_name: String = default_values.string, tid: Int = default_values.int)
case class expert_info_for_feedback(raw: String = default_values.string, pref_label: String = default_values.string, source: String = default_values.string, code: String = default_values.string, iri: String = default_values.string, description: String = default_values.string)

object Tables {

  class ontology (tag: Tag) extends Table[ontology_type](tag, Some("public"), "ontology"){
    def source = column[String]("source",O.SqlType("VARCHAR(64)"), O.PrimaryKey)
    def title = column[Option[String]]("title",O.SqlType("VARCHAR(64)"))
    def description = column[Option[String]]("description")
    def url = column[Option[String]]("url",O.SqlType("VARCHAR(128)"))

    def * = (source,title,description,url) <> (ontology_type.tupled, ontology_type.unapply)
  }
  val ontology = TableQuery[ontology]

  class vocabulary(tag: Tag) extends Table[vocabulary_type](tag, Some("public"), "vocabulary"){
    def tid = column[Int]("tid", O.AutoInc, O.PrimaryKey)
    def source = column[String]("source", O.SqlType("VARCHAR(32)"))
    def code = column[String]("code",O.SqlType("VARCHAR(64)"))
    def pref_label = column[String]("pref_label", O.SqlType("VARCHAR(128)"))
    def description = column[String]("description")
    def iri = column[String]("iri")

    def idx = index("vocabulary_source_code_key",(source,code),unique = true)
    def fk = foreignKey("ontology_source_fk",source,ontology)(_.source, onDelete = ForeignKeyAction.Cascade)

    def * = (tid, source,code,pref_label,description,iri) <> (vocabulary_type.tupled, vocabulary_type.unapply)
  }
  val vocabulary = TableQuery[vocabulary]

  class synonym(tag: Tag) extends Table[synonym_type](tag,Some("public"), "synonym"){
    def tid = column[Int]("tid")
    def label = column[String]("pref_label", O.SqlType("VARCHAR(128)"))
    def ttype = column[String]("type", O.SqlType("VARCHAR(4)"))

    def pk = primaryKey("synonym_pkey", (tid,label,ttype))
    def fk = foreignKey("vocabulary_syn_fk",tid, vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def idx = index("syn_label_idx", label)

    def * = (tid, label,ttype) <> (synonym_type.tupled, synonym_type.unapply)
  }
  val synonym = TableQuery[synonym]

  class reference(tag: Tag) extends Table[reference_type](tag, Some("public"), "reference"){
    def tid = column[Int]("tid")
    def source = column[String]("source", O.SqlType("VARCHAR(128)"))
    def code = column[String]("code")

    def pk = primaryKey("reference_pkey", (tid, source, code))
    def fk = foreignKey("vocabulary_xref_fk",tid, vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("xref_code_idx",code)

    def * = (tid, source, code) <> (reference_type.tupled, reference_type.unapply)
  }
  val reference = TableQuery[reference]

  class raw_annotation(tag: Tag) extends Table[raw_annotation_type](tag, Some("public"),"raw_annotation"){
    def tid = column[Int]("tid")
    def label = column[String]("pref_label", O.SqlType("VARCHAR(128)"))
    def table_name = column[String]("table_name", O.SqlType("VARCHAR(32)"))
    def column_name = column[String]("column_name", O.SqlType("VARCHAR(32)"))
    def method = column[Char]("method")

    def pk = primaryKey("raw_pkey",(label,table_name,column_name))
    def fk = foreignKey("vocabulary_raw_fk",tid,vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def * = (tid,label,table_name,column_name,method) <> (raw_annotation_type.tupled, raw_annotation_type.unapply)
  }
  val raw_annotation = TableQuery[raw_annotation]


  class relationship(tag: Tag) extends Table[relationship_type](tag, Some("public"), "relationship"){
    def tid_p = column[Int]("tid_parent")
    def tid_c = column[Int]("tid_child")
    def rel_type = column[String]("rel_type", O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("relationship_pkey",(tid_p,tid_c,rel_type))
    def fk_p = foreignKey("vocabulary_rel_p_fk",tid_p,vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_c = foreignKey("vocabulary_rel_c_fk",tid_c,vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("rel_idx",tid_c)

    def * = (tid_p, tid_c, rel_type) <> (relationship_type.tupled, relationship_type.unapply)
  }
  val relationship = TableQuery[relationship]

  class relationship_unfolded(tag: Tag) extends  Table[relationship_unfolded_type](tag, Some("public"),"relationship_unfolded"){
    def tid_a = column[Int]("tid_ancestor")
    def tid_d = column[Int]("tid_descendant")
    def distance = column[Int]("distance")
    def rel_type = column[String]("rel_type", O.SqlType("VARCHAR(8)"))

    def pk = primaryKey("relationship_unfolded_pk",(tid_a,tid_d,rel_type))
    def fk_a = foreignKey("vocabulary_rel_a_unfolded",tid_a,vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def fk_d = foreignKey("vocabulary_rel_d_unfolded",tid_d,vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)

    def idx = index("rel_unfolded_idx", tid_d)

    def * = (tid_a,tid_d,distance,rel_type) <> (relationship_unfolded_type.tupled, relationship_unfolded_type.unapply)
  }
  val relationship_unfolded = TableQuery[relationship_unfolded]

  class expert_choice(tag: Tag) extends Table[expert_choice_type](tag, "expert_choice"){
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def raw_value = column[String]("raw_value")
    def tid = column[Option[Int]]("tid")
    def parsed_value = column[Option[String]]("parsed_value")
    def label = column[Option[String]]("pref_label")
    def source = column[Option[String]]("source")
    def code = column[Option[String]]("code")
    def resolved = column[Boolean]("resolved", O.Default(false))
    def iri = column[Option[String]]("iri")
    def provenance = column[String]("provenance")
    def timestamp = column[String]("timestamp")

    def idx = index("user_feedback_idx",(tid,raw_value,source,code),unique = true)

    def * = (id, resolved, table_name, column_name, tid, raw_value, parsed_value, label, source, code, iri, provenance,timestamp) <> (expert_choice_type.tupled, expert_choice_type.unapply)
  }
  val expert_choice = TableQuery[expert_choice]

  class expert_preference(tag: Tag) extends Table[expert_preference_type](tag, "expert_preference"){
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def raw_value = column[String]("raw_value")
    def source = column[String]("source")
    def code = column[String]("code")
    def * = (id, table_name, column_name, raw_value, source, code) <> (expert_preference_type.tupled,expert_preference_type.unapply)
  }
  val expert_preference = TableQuery[expert_preference]

  class expert_feedback(tag: Tag) extends  Table[expert_feedback_type](tag, "expert_feedback"){
    def expert_username = column[String]("expert_username")
    def raw_value = column[String]("raw_value")
    def table_name = column[String]("table_name")
    def column_name = column[String]("column_name")
    def tid = column[Int]("tid")
    //1 EXACT
    //2 ALMOST EXACT
    //3 ACCEPTABLE
    //4 BAD
    def rating = column[Int]("rating")

    def fk = foreignKey("expert_feedback_fk",tid,vocabulary)(_.tid, onDelete = ForeignKeyAction.Cascade)
    def * = (expert_username,raw_value,table_name,column_name,tid) <> (expert_feedback_type.tupled, expert_feedback_type.unapply)
  }
  val expert_feedback = TableQuery[expert_feedback]
}
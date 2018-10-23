package enricher.engine

import java.sql.BatchUpdateException

import OlsInterface._
import config_pkg.ApplicationConfig
import enricher.dbcon.DbHandler._
import enricher.dbcon._
import org.slf4j.LoggerFactory
import utilities.Utils.get_timestamp

object RelationType extends Enumeration {
  type ttype = Value
  val IS_A, PART_OF = Value
}

object SynonymType extends Enumeration {
  type ttype = Value
  val SYN, RELATED = Value
}

case class Synonym(label: String, ttype: SynonymType.ttype)

case class Relation(term: Either[Term, String], ttype: RelationType.ttype)

case class Xref(source: String, code: String, url: Option[String])

case class ScoredTerm(term: Term, score: Double)

case class RawValue(value: String, table: String, column: String)

/**
  *
  * @param rawValue
  * @param ontology
  * @param code
  * @param iri
  * @param prefLabel
  * @param description
  * @param synonyms
  * @param xref
  * @param parents
  * @param children
  * @param depth
  * @param tid
  */
case class Term(ontology: ontology_type,
                code: String,
                iri: String,
                rawValue: Option[RawValue] = None,
                prefLabel: Option[String] = None,
                description: Option[String] = None,
                synonyms: Option[List[Synonym]] = None,
                xref: Option[List[Xref]] = None,
                parents: Option[List[Relation]] = None,
                children: Option[List[Relation]] = None,
                depth: Int = 0,
                tid: Option[Int] = None) {

  //SAVE TERM TO LOCAL KB AND ASSIGN TID
  def saveToKB(): Term = {

    val existing = Term.loadFromKB(this)

    if (existing.tid.isDefined) {
      if (existing.rawValue.isDefined) {
        val raw = raw_annotation_type(
          existing.tid.get,
          existing.rawValue.get.value,
          existing.rawValue.get.table,
          existing.rawValue.get.column,
          'L'
        )
        raw_insert(List(raw))
        update_tid(existing.rawValue.get, existing.tid)
      }
      existing
    }
    else {
      if (!onto_exist(this.ontology.source))
        insert_ontology(this.ontology)

      val vocabulary = vocabulary_type(-1, this.ontology.source, this.code, this.prefLabel.get, this.description.get, this.iri)

      val new_tid = vocabulary_insert(vocabulary)

      val synonyms = this.synonyms.get.map(a =>
        synonym_type(
          -1,
          new_tid,
          a.label,
          a.ttype.toString
        )
      )

      val references = this.xref.get.map(a =>
        reference_type(
          -1,
          new_tid,
          a.source,
          a.code,
          a.url
        )
      )

      if (this.rawValue.isDefined) {
        val raw = raw_annotation_type(
          new_tid,
          this.rawValue.get.value,
          this.rawValue.get.table,
          this.rawValue.get.column,
          'O'
        )
        raw_insert(List(raw))
        update_tid(this.rawValue.get, Some(new_tid))
      }

      synonym_insert(synonyms)
      reference_insert(references)
      this.copy(tid = Some(new_tid))
    }
  }

  //FILL OPTIONAL FIELDS OF TERM
  def fill(): Term = Term.fill(this.ontology.source, this.code, this.iri)

  def fill_relation(): Term = {
    this.rec_parents(0).rec_children(0)
  }

  def rec_parents(depth: Int): Term = {

    val parents_uncomplete = this.parents.get.flatMap(get_relatives)

    val parents_complete = parents_uncomplete.map(a => Relation(Left(a.term.left.get.fill()), a.ttype))

    val parents_complete_tid = parents_complete.map(a => Relation(Left(a.term.left.get.saveToKB()), a.ttype))

    val max_depth = 2 //ApplicationConfig.get_anc_limit()

    val p =
      if (depth < max_depth) {
        parents_complete_tid.map{a =>
          val new_term = a.term.left.get.rec_parents(depth+1)
          val rel_type = a.ttype
          Relation(Left(new_term),rel_type)
        }
      }
      else {
        parents_complete_tid
      }
    this.copy(parents = Some(p))
  }
  
  def rec_children(depth: Int): Term = {

    val children_uncomplete = this.children.get.flatMap(get_relatives)

    val children_complete = children_uncomplete.map(a => Relation(Left(a.term.left.get.fill()), a.ttype))

    val children_complete_tid = children_complete.map(a => Relation(Left(a.term.left.get.saveToKB()), a.ttype))

    val max_depth = 2 //ApplicationConfig.get_anc_limit()

    val p =
      if (depth < max_depth) {
        children_complete_tid.map{a =>
          val new_term = a.term.left.get.rec_children(depth+1)
          val rel_type = a.ttype
          Relation(Left(new_term),rel_type)
        }
      }
      else {
        children_complete_tid
      }
    this.copy(children = Some(p))
  }

  def save_relation(): Unit = {
    val tid_current = this.tid.get

    this.parents.get.foreach(a =>
    if (a.term.isLeft) {
        hyp_insert(relationship_type(a.term.left.get.tid.get, tid_current, a.ttype.toString))
        a.term.left.get.save_relation()
      }
    )
    this.children.get.foreach(a =>
      if (a.term.isLeft) {
        hyp_insert(relationship_type(tid_current,a.term.left.get.tid.get, a.ttype.toString))
        a.term.left.get.save_relation()
      }
    )
  }

  override def toString: String = {
    "Source: "+this.ontology.source+"\n"+
    "Code: "+this.code+"\n"+
    "Iri: "+this.iri+"\n"
  }
}

object Term {

  def fill(source: String, code: String, iri: String): Term = {
    ols_get_info(source, code, iri)
  }

  //LOAD TERM FROM KB AND ASSIGN TID IF TERM EXISTS, OTHERWISE RETURNS TERM
  def loadFromKB(term: Term): Term = {
    val existing_tid = get_tid_option(term.ontology.source, term.code)
    if (existing_tid.isDefined) {
      term.copy(tid = existing_tid)
    }
    else term
  }
}

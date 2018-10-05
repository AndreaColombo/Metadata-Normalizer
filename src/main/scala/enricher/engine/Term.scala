package enricher.engine

import java.sql.BatchUpdateException

import Ols_interface._
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
      val raw = raw_annotation_type(
        existing.tid.get,
        existing.rawValue.get.value,
        existing.rawValue.get.table,
        existing.rawValue.get.column,
        'L'
      )
      raw_insert(List(raw))
      update_tid(existing.rawValue.get,existing.tid)
      existing
    }
    else {
      if (!onto_exist(this.ontology.source))
        insert_ontology(this.ontology)
      val vocabulary = vocabulary_type(-1, this.ontology.source, this.code, this.prefLabel.get, this.description.get, this.iri)

      val new_tid = vocabulary_insert(vocabulary)

      val synonyms = this.synonyms.get.map(a =>
        synonym_type(
          new_tid,
          a.label,
          a.ttype.toString
        )
      )

      val references = this.xref.get.map(a =>
        reference_type(
          new_tid,
          a.source,
          a.code,
          a.url
        )
      )

      val raw = raw_annotation_type(
        new_tid,
        this.rawValue.get.value,
        this.rawValue.get.table,
        this.rawValue.get.column,
        'O'
      )

      synonym_insert(synonyms)
      reference_insert(references)
      raw_insert(List(raw))
      update_tid(this.rawValue.get,Some(new_tid))
      this.copy(tid = Some(new_tid))
    }
  }

  //FILL OPTIONAL FIELDS OF TERM
  def fill(): Term = Term.fill(this.ontology.source, this.code, this.iri)

  def get_user_feedback(): List[expert_choice_type] = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val value = this.rawValue.get.value
    logger.info(s"Value $value, best match not found in online KB, user feedback")
    ols_get_user_feedback(this.rawValue.get)
  }
}

object Term {

  def fill(source: String, code: String, iri: String): Term = {
    ols_get_info(source,code,iri)
  }

  def save_user_feedback(rows: List[expert_choice_type]): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    try {
      DbHandler.user_feedback_insert(rows)
    }
    catch {
      case e: BatchUpdateException => logger.info("User feedback exception", e.getNextException)
    }
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

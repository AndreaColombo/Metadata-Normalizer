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

    //TODO ADD CHECK EXISTENCE OF TERM IN KB maybe done
    val vocabulary = vocabulary_type(-1, this.ontology.source, this.code, this.prefLabel.get, this.description.get, this.iri)
    if (!onto_exist(this.ontology.source))
      insert_ontology(this.ontology)

    val existing_tid = get_tid_option(this.ontology.source, this.code)

    if (existing_tid.isDefined) {
      this.copy(tid = existing_tid)
    }
    else {
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
      this.copy(tid = Some(new_tid))
    }
  }

  //FILL OPTIONAL FIELDS OF TERM
  def fill(): Term = Term.fill(this.ontology.source, this.iri)

  def get_user_feedback(): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val value = this.rawValue.get.value
    var user_feedback: List[expert_choice_type] = List()
    user_feedback = ols_get_user_feedback(this.rawValue.get)
    try {
      DbHandler.user_feedback_insert(user_feedback)
      logger.info(s"Value $value, best match not found in online KB, user feedback")
    }
    catch {
      case e: BatchUpdateException => logger.info("User feedback exception", e.getNextException)
    }
  }
}

object Term {


  //FILL OPTIONAL FIELDS OF TERM
//  def fill(ontology: String, code: String): Term = fill(Term(ontology,code,Ols_interface.ols_get_iri(ontology,code)))

  def fill(source: String, iri: String): Term = {
    ols_get_info(source,iri)
  }

  //LOAD TERM FROM KB
  def loadFromKB(source: String, code: String): Term = ???

}

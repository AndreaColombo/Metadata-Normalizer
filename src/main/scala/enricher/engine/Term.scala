package enricher.engine

import Ols_interface._

object RelationType extends Enumeration {
  type ttype = Value
  val IS_A, PART_OF = Value
}

object SynonymType extends Enumeration {
  type ttype = Value
  val EXACT, RELATED = Value
}

case class Synonym(label: String, ttype: SynonymType.ttype)

case class Relation(term: Either[Term, String], ttype: RelationType.ttype)

case class Xref(source: String, code: String, url: Option[String])

case class ScoredTerm(term: Term, score: Double)

case class RawValue(value: String, table: String, column: String)
/**
  *
  * @param rawValue
  * @param source
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
case class Term(source: String,
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
    val new_tid = 0
    this.copy(tid=Some(new_tid))
  }

  //FILL OPTIONAL FIELDS OF TERM
  def fill(): Term = Term.fill(this.source,this.iri)
}

object Term {
  //FILL OPTIONAL FIELDS OF TERM
//  def fill(source: String, code: String): Term = fill(Term(source,code,Ols_interface.ols_get_iri(source,code)))

  def fill(source: String, iri: String): Term = {
    ols_get_info(source,iri)
  }

  //LOAD TERM FROM KB
  def loadFromKB(source: String, code: String): Term = ???
}

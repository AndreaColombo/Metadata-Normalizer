package enricher.engine

object RelationType extends Enumeration {
  type ttype = Value
  val IS_A, PART_OF = Value
}

object SynonymType extends Enumeration {
  type ttype = Value
  val EXACT, RELATED = Value
}

case class Synonym(label: String, ttype: SynonymType.ttype)

case class Relation(term: Term, ttype: RelationType.ttype)

case class Xref(source: String, code: String)

case class ScoredTerm(term: Term, score: Double)

case class Term(var source: String, var code: String, iri: String, prefLabel: Option[String], description: Option[String], synonyms: Option[List[Synonym]], xref:Option[List[Xref]], parents: Option[List[Relation]], children: Option[List[Relation]], depth: Int = 0, rawValue: Option[List[String]], tid: Option[Int]) {

  //SAVE TERM TO LOCAL KB AND RETURNS ASSIGNED TID
  def saveToKB(): Int = ???

  //FILL OPTIONAL FIELDS OF TERM
  def fill() = ???

  object Term {
    //FILL OPTIONAL FIELDS OF TERM
    def fill(source: String, code: String) = ???

    //LOAD TERM FROM KB
    def loadFromKB(source: String, code: String) = ???
  }
}

package enricher.engine

import enricher.dbcon.DbHandler._
import enricher.dbcon._
import enricher.engine.OlsInterface._

/**
  * Enumeration for the relations type
  */
object RelationType extends Enumeration {
  type ttype = Value
  val IS_A, PART_OF = Value
}

/**
  * Enumeration for the synonyms type
  */
object SynonymType extends Enumeration {
  type ttype = Value
  val SYN, RELATED = Value
}

/**
  * Case class for a term synonym
  * @param label The label of the synonym
  * @param ttype Type of synonym, either exact or related
  */
case class Synonym(label: String, ttype: SynonymType.ttype)

/**
  * Case class for a term's parents or children
  * @param term Either a string that is the url containing the info of the term parents/children or a Term
  * @param ttype Type of the relation, of type RelationType
  */
case class Relation(term: Either[Term, String], ttype: RelationType.ttype)

/**
  * Case class for a term cross reference
  * @param source Source of the cross reference
  * @param code Code of the cross reference
  * @param url Url of the cross reference
  */
case class Xref(source: String, code: String, url: Option[String])

/**
  * Case class containing a term annotated and the score associated to that annotation
  * @param term
  * @param score
  */
case class ScoredTerm(term: Term, score: Double)

/**
  * Case class for a raw value
  * @param value Text value
  * @param table The table in the database where the value is stored
  * @param column The column in the database where the value is stored
  */
case class RawValue(value: String, table: String, column: String)

/**
  * The main case class of Enricher, define the element Term which
  * @param ontology
  * @param code
  * @param iri
  * @param rawValue
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

  /**
    * If term is not present in the database save it to local KB and assign tid, then proceed to save in the corresponding table all the term attributes
    * If term is already present assign tid
    * In both cases save tid to corresponding GCM table
    * @return Term with tid assigned
    */
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
      ):+synonym_type(-1,new_tid,this.prefLabel.get,"PREF")

      val references = this.xref.get.map{a =>
        insert_ontology(ols_get_onto_info(a.source))
        reference_type(
            -1,
            new_tid,
            a.source,
            a.code,
            a.url
        )
      }:+reference_type(-1,new_tid,this.ontology.source,this.code,Some(this.iri))

      if (this.rawValue.isDefined) {
        val raw = raw_annotation_type(
          new_tid,
          this.rawValue.get.value,
          this.rawValue.get.table,
          this.rawValue.get.column,
          'O'
        )
        raw_insert(List(raw))
        synonym_insert(List(synonym_type(-1,new_tid,this.rawValue.get.value,"RAW")))
        update_tid(this.rawValue.get, Some(new_tid))
      }

      synonym_insert(synonyms)
      reference_insert(references)
      this.copy(tid = Some(new_tid))
    }
  }

  /**
    * Fill all the optional attributes of Term
    * @return Term completed
    */
  def fill(): Term = Term.fill(this.ontology.source, this.code, this.iri)

  /**
    * Retrieve parents and children of a root term
    * @return Term with relations complete
    */
  def fill_relation(): Term = {
    this.rec_parents(0).rec_children(0)
  }

  /**
    * Recursive function to retrieve all parents of a term
    * @param depth Level of the hierarchical tree
    * @return Term with parents complete
    */
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

  /**
    * Recursive function to retrieve all children of a term
    * @param depth Level of the hierarchical tree
    * @return Term with children complete
    */
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

  /**
    * Save parents and children in the corresponding table: relation table
    */
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

/**
  * This object contains the static methods of term
  */
object Term {

  /**
    * Call ols get info to fill in the optional field of Term
    * @param source Source of the term
    * @param code Code of the Term
    * @param iri Iri of the term
    * @return Term completed
    */
  def fill(source: String, code: String, iri: String): Term = {
    ols_get_info(source, code, iri)
  }

  /**
    * Load term from local kb and assign tid if term exists
    */
  def loadFromKB(term: Term): Term = {
    val existing_tid = get_tid_option(term.ontology.source, term.code)
    if (existing_tid.isDefined) {
      term.copy(tid = existing_tid)
    }
    else term
  }
}

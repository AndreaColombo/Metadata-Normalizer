package enricher.engine

import config_pkg.ApplicationConfig
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
  val SYN, RELATED, BROAD, ADJ = Value
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
  * @param term term
  * @param score Score of the annotation
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
  * The main case class of Enricher, define the element Term which is built to resemble the term in ols
  * Is also built to mirror the local knowledge base structure
  * @param ontology Element of case class ontology, mirrors the table ontology of the KB
  * @param code String element which identifies the ontological ID of the term
  * @param iri Iri of the term
  * @param rawValue Element of case class RawValue, contains the GCM info of the value
  * @param prefLabel Preferred label of the term
  * @param description Description of the term
  * @param synonyms Synonyms of the term
  * @param xref Cross references of the term
  * @param parents Parents of the term
  * @param children Parents of the term
  * @param depth Int number signifying the hierachy in the relation tree, for root Terms it's zero
  * @param tid Tid assigned by the database engine to the term when inserting it into the local knowledge base, thus it exists only after the term is inserted
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
        synonym_insert(List(synonym_type(-1,existing.tid.get,this.rawValue.get.value,"RAW")))
        raw_insert(raw)
        update_gcm_tid(existing.rawValue.get, existing.tid)
      }
      existing
    }
    else {

      insert_ontology(ontology(this.ontology.source.toLowerCase,this.ontology.title,this.ontology.description,this.ontology.url))

      val vocabulary = vocabulary_type(-1, this.ontology.source.toLowerCase, this.code, this.prefLabel.get, this.description.get, this.iri)

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
        insert_ontology(ols_get_onto_info(a.source.toLowerCase))
        reference_type(
            -1,
            new_tid,
            a.source.toLowerCase,
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
        raw_insert(raw)
        synonym_insert(List(synonym_type(-1,new_tid,this.rawValue.get.value,"RAW")))
        update_gcm_tid(this.rawValue.get, Some(new_tid))
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
  def fill(): Term = ols_get_info(this.ontology.source, this.code, this.iri)

  /**
    * Retrieve parents and children of a root term
    * @return Term with relations complete
    */
  def fill_relation(): Term = {
    this.rec_parents(this.depth).rec_children(this.depth)
  }

  /**
    * Recursive function to retrieve all parents of a term
    * @param depth Level of the hierarchical tree
    * @return Term with parents complete
    */
  def rec_parents(depth: Int): Term = {

    val parents_uncomplete = this.parents.get.flatMap(get_relatives)

    val parents_complete = parents_uncomplete.map(a => Relation(Left(a.term.left.get.fill()), a.ttype))

    val max_depth = ApplicationConfig.get_anc_limit()

    val p =
      if (depth < max_depth) {
        parents_complete.map{a =>
          val new_term = a.term.left.get.rec_parents(depth+1)
          val rel_type = a.ttype
          Relation(Left(new_term),rel_type)
        }
      }
      else {
        parents_complete
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

    val max_depth = ApplicationConfig.get_desc_limit()

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
        val term_tid = a.term.left.get.saveToKB()
        val tid_parent = term_tid.tid.get
        hyp_insert(relationship_type(tid_parent, tid_current, a.ttype.toString))
        term_tid.save_relation()
      }
    )

    this.children.get.foreach(a =>
      if (a.term.isLeft) {
        val term_tid = a.term.left.get.saveToKB()
        val tid_child = term_tid.tid.get
        hyp_insert(relationship_type(tid_current,tid_child, a.ttype.toString))
        term_tid.save_relation()
      }
    )
  }

//  def unfold_relationships(current_tid: Int, distance: Int): Unit = {
//
//    this.parents.get.foreach{a =>
//      if(a.term.isLeft){
//        val term_tid = a.term.left.get.tid.get
//
//      }
//    }
//  }

  override def toString: String = {
    "\n"+
    "Source: "+this.ontology.source+"\n"+
    "Code: "+this.code+"\n"+
    "Iri: "+this.iri+"\n"+
    "Label: "+this.prefLabel+"\n"+
    "Raw value: "+this.rawValue+"\n"
  }
}

/**
  * This object contains the static methods of term
  */
object Term {

  /**
    * Load term from local kb and assign tid if term exists
    */
  def loadFromKB(term: Term): Term = {
    val existing_tid = get_tid_option(term.ontology.source.toLowerCase, term.code)
    if (existing_tid.isDefined) {
      term.copy(tid = existing_tid)
    }
    else term
  }
}

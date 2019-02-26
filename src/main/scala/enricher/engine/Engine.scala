package enricher.engine


import config_pkg.ApplicationConfig
import enricher.dbcon.Tables._
import enricher.dbcon._
import enricher.engine.OlsInterface.{get_score, ols_search_term, ols_get_status, ols_get_user_feedback}
import utilities.Utils.get_timestamp
import org.slf4j.LoggerFactory
import slick.jdbc.PostgresProfile.api._

/**
  * Enricher engine, contains only one method which is the controller of the whole metadata enricher system
  */
object Engine {

  /**
    * Controller method of enricher engine, called by main
    * @param table_name Table of the gcm from where the engine takes the data to annotate
    * @param column_name Column of the gcm from where the engine takes the data to annotate
    */
  def controller(table_name: String,column_name: String): Unit = {
    val logger = LoggerFactory.getLogger(this.getClass)
    val threshold = ApplicationConfig.get_threshold()
    DbHandler.clean_user_feedback(table_name,column_name)
    val raw_values = DbHandler.get_raw_values(table_name,column_name)
    logger.info(column_name)
    logger.info(raw_values.toString())
    for (raw_value <- raw_values) {
      val condition = (a: raw_annotation) => a.table_name === table_name && a.column_name === column_name && a.label.toLowerCase === raw_value.toLowerCase
      val result_raw = DbHandler.get_raw_annotation(condition)
      val result_syn = DbHandler.get_synonym_by_value(raw_value.map(_.toLower))
      val result_user_changes = DbHandler.get_raw_expert_preference(table_name, column_name, raw_value.map(_.toLower))
      val rv = RawValue(raw_value,table_name,column_name)

      //LOCAL KB LOOKUP
      if (result_raw.tid != default_values.int) {
        //VALUE FOUND RAW
        logger.info(s"""Value "$raw_value" found as RAW in local KB""")
        DbHandler.update_gcm_tid(rv, Some(result_syn.tid))
      }
      else if (result_user_changes._1 != "null") {
        //VALUE FOUND IN USER CHANGES
        logger.info(s"""Value "$raw_value" found in user changes""")
        val source = result_user_changes._1
        val onto = OlsInterface.ols_get_onto_info(source)
        val code = result_user_changes._2
        val iri = OlsInterface.ols_get_iri(source, code)
        if (!DbHandler.onto_exist(onto.source)) {
          DbHandler.insert_ontology(onto)
        }
        val term = Term(onto, code, iri)
        try {
          term.fill().fill_relation().saveToKB().save_relation()
        }
        catch {
          case e: Exception => logger.info("Error in term retrieval "+term.code)
        }
      }
      else if(result_syn.tid != default_values.int) {
        //VALUE FOUND PREF OR SYN
        if (result_syn.ttype == "pref") {
          logger.info(s"""Value "$raw_value" found as PREF in local KB""")
          val suggestion = DbHandler.get_vocabulary_by_tid(result_syn.tid)
          if (!DbHandler.expert_choice_exist(raw_value, suggestion.source, suggestion.code)) {
            DbHandler.user_feedback_insert(List(expert_choice_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:PREF", get_timestamp())))
          }
        }
        else if (ApplicationConfig.is_local_syn_enabled){
          logger.info(s"""Value "$raw_value" found as SYN in local KB""")
          val suggestion = DbHandler.get_vocabulary_by_tid(result_syn.tid)
          if (!DbHandler.expert_choice_exist(raw_value, suggestion.source, suggestion.code)) {
            DbHandler.user_feedback_insert(List(expert_choice_type(default_values.int, default_values.bool, table_name, column_name, Some(result_syn.tid), raw_value, null, Some(suggestion.label), Some(suggestion.source), Some(suggestion.code), Some(suggestion.iri), "LOCAL:SYN", get_timestamp())))
          }
        }
      }
      //ONLINE KB LOOKUP
      else {
        val terms_searched = ols_search_term(rv).map(_.fill())

        val terms_scored = terms_searched.map(_.copy(rawValue = Some(rv))).map(a =>
          if (a.prefLabel.isDefined)
            ScoredTerm(a, get_score(a.rawValue.get.value, a.prefLabel.get, a.synonyms.get.map(_.label)))
          else ScoredTerm(a, Double.NegativeInfinity)
        )

        terms_scored.foreach(a => logger.info(a.toString))

        val terms_ordered = terms_scored.sortWith(_.score > _.score)

        val terms_filtered = terms_ordered.filter(_.score > threshold)

        //BEST MATCH FOUND
        if (terms_filtered.nonEmpty) {
          val best_term = terms_filtered.head
          val matchModeRandom = ApplicationConfig.get_search_mode()
          if (!matchModeRandom) {
            val best_terms = terms_filtered.filter(_.score == best_term.score)
            //BEST MATCH UNDECIDED
            if (best_terms.length > 1) {
              logger.info(s"""Best match undecided for value "$raw_value"""")
              best_terms.foreach(a =>
                DbHandler.user_feedback_insert(List(expert_choice_type(
                  default_values.int,
                  default_values.bool,
                  a.term.rawValue.get.table,
                  a.term.rawValue.get.column,
                  tid = None,
                  a.term.rawValue.get.value,
                  parsed_value = None,
                  a.term.prefLabel,
                  Some(a.term.ontology.source),
                  Some(a.term.code),
                  Some(a.term.iri),
                  "ONLINE:UNDECIDED " + a.score.toString,
                  get_timestamp()))
                )
              )
            }
            //BEST MATCH DECIDED
            else {
              logger.info(s"""Value "$raw_value" best match found in online KB with match mode not random""")
              try {
                best_term.term.fill_relation().saveToKB().save_relation()
              }
              catch {
                case e: Exception => logger.info("Error in term retrieval " + best_term.term.code)
              }
            }
          }
          //BEST MATCH FOUND WITH MATCH MODE RANDOM
          else {
            logger.info(s"""Value "$raw_value" best match found in online KB with match mode random""")
            try {
              best_term.term.fill_relation().saveToKB().save_relation()
            }
            catch {
              case e: Exception => logger.info("Error in term retrieval "+best_term.term.code)
            }
          }
        }
        //BEST MATCH NOT FOUND, USER FEEDBACK
        else {
          DbHandler.user_feedback_insert(terms_ordered.map(a =>
            if(a.term.prefLabel.isDefined) {
              expert_choice_type(
                id = -1,
                resolved = false,
                a.term.rawValue.get.table,
                a.term.rawValue.get.column,
                tid = None,
                a.term.rawValue.get.value,
                parsed_value = None,
                a.term.prefLabel,
                Some(a.term.ontology.source),
                Some(a.term.code),
                Some(a.term.iri),
                "ONLINE:LOW " + a.score.toString,
                get_timestamp())
            }
            else {
              expert_choice_type(
                id = -1,
                resolved = false,
                a.term.rawValue.get.table,
                a.term.rawValue.get.column,
                tid = None,
                a.term.rawValue.get.value,
                parsed_value = None,
                a.term.prefLabel,
                Some(a.term.ontology.source),
                Some(a.term.code),
                Some(a.term.iri),
                "ONLINE:ERROR "+ols_get_status(a.term.ontology.source,a.term.iri),
                get_timestamp())
            }
          ))
          val user_fb_rows = ols_get_user_feedback(rv)
          DbHandler.user_feedback_insert(user_fb_rows)
        }
      }
    }
  }
}
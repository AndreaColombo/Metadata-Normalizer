package config_pkg

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import scala.collection.JavaConverters._

/**
  * This object contains methods get configuration options from application.conf
  */
object ApplicationConfig  {

  val parsedConfig: Config = ConfigFactory.parseFile(new File("src/main/scala/config_pkg/application.conf"))
  val conf: Config = ConfigFactory.load(parsedConfig)

  /**
    * Get the threshold for a match to be good
    * @return
    */
  def get_threshold(): Double = conf.getDouble("threshold_match")

  /**
    * Get the table to which the column belongs
    * @param column Column to get provenance
    * @return
    */
  def get_table_by_column(column: String): String = {
    val table_list = conf.getObject("db_config").keySet()
    var table = ""
    for (elem <- table_list.asScala) {
      val t = get_termtype_list(elem)
      if (t.exists(_.equals(column)))
        table = elem
    }
    table
  }

  /**
    * Get ontologies chosen for annotating a specific term type
    * @param term_type Term type
    * @return A list of ontologies
    */
  def get_ontologies_by_type(term_type: String): List[String] = {
    val table = get_table_by_column(term_type)
    conf.getStringList(s"db_config.$table.$term_type.ontologies").asScala.toList
  }

  /**
    * Retrieves all columns of a table of GCM
    * @param table Table to retrieve columns
    * @return A list of columns
    */
  def get_termtype_list(table: String): List[String] = conf.getObject(s"db_config.$table").keySet().asScala.toList

  /**
    * Return the limit of parents retrieval
    * @return
    */
  def get_anc_limit(): Int = conf.getInt("ontology_depth.anc_depth")

  /**
    * Return the limit of children retrieval
    * @return
    */
  def get_desc_limit(): Int = conf.getInt("ontology_depth.desc_depth")

  /**
    * Return the list of all gcm tables
    * @return
    */
  def get_gcm_table_list(): List[String] = conf.getObject("db_config").keySet().asScala.toList

  /**
    * Return bioportal apikey
    * @return
    */
  def get_bp_apikey(): String = conf.getString("bioportal_apikey")

  /**
    * Transform a text score into its corresponding numeric score
    * @param match_type Text score
    * @return
    */
  def get_match_score(match_type: String): Int = conf.getInt("scores.match_score_"+match_type.toLowerCase)

  /**
    * Returns numeric modifiers for words distance function
    * @param mod Text modifier
    * @return
    */
  def get_modifier(mod: String): Double = conf.getDouble("score_modifiers."+mod)

  /**
    * Returns true if match mode is set to random, false otherwise
    * @return
    */
  def get_search_mode(): Boolean = conf.getBoolean("match_mode_random")

  /**
    * Returns maximum set size for ontologies set calculator
    * @return
    */
  def get_best_onto_limit_for_set(): Int = conf.getInt("best_onto_limit_for_set")

  /**
    * Returns minimum coverage for a set of ontology when calculating ontologies sets
    * @return
    */
  def get_best_onto_coverage_threshold_for_set(): Double = conf.getDouble("get_best_onto_coverage_threshold_for_set")
}


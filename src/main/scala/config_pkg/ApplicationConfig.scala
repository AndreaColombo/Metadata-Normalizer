package config_pkg

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import scala.collection.JavaConverters._

object ApplicationConfig  {

  val parsedConfig: Config = ConfigFactory.parseFile(new File("src/main/scala/config_pkg/application.conf"))
  val conf: Config = ConfigFactory.load(parsedConfig)

  def get_threshold() = conf.getDouble("threshold_match")

  def get_table_by_column(term_type: String): String = {
    val table_list = conf.getObject("db_config").keySet()
    var table = ""
    for (elem <- table_list.asScala) {
      val t = get_termtype_list(elem)
      if (t.exists(_.equals(term_type)))
        table = elem
    }
    table
  }

  def get_ontologies_by_type(term_type: String): List[String] = {
    val table = get_table_by_column(term_type)
    conf.getStringList(s"db_config.$table.$term_type.ontologies").asScala.toList
  }

  def get_termtype_list(table: String): List[String] = conf.getObject(s"db_config.$table").keySet().asScala.toList

  def get_anc_limit(): Int = conf.getInt("ontology_depth.anc_depth")

  def get_desc_limit(): Int = conf.getInt("ontology_depth.desc_depth")

  def get_gcm_table_list(): List[String] = conf.getObject("db_config").keySet().asScala.toList

  def get_bp_apikey(): String = conf.getString("bioportal_apikey")

  def get_score(match_type: String): Int = conf.getInt("scores.match_score_"+match_type.toLowerCase)

  def get_excess_words_penalty(): Int = conf.getInt("scores.excess_words_penalty")

  def get_modifier(mod: String): Double = conf.getDouble("score_modifiers."+mod)

  def get_search_mode(): Boolean = conf.getBoolean("match_mode_random")

  def get_best_onto_limit_for_set(): Int = conf.getInt("best_onto_limit_for_set")

  def get_best_onto_coverage_threshold_for_set(): Double = conf.getDouble("get_best_onto_coverage_threshold_for_set")
}


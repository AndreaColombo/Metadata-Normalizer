package Config

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import scala.collection.JavaConverters._

object config  {

  val parsedConfig: Config = ConfigFactory.parseFile(new File("src/main/scala/Config/application.conf"))
  val conf: Config = ConfigFactory.load(parsedConfig)

  def get_threshold():Int = conf.getInt("threshold_match")

  def get_ontologies_by_type(term_type: String): String = conf.getString("ontologies_termtype_map."+term_type)

  def get_termtype_list(table: String): List[String] = conf.getStringList("table_column_map."+table).asScala.toList

  def get_anc_limit(): Int = conf.getInt("ontology_depth.anc_deph")

  def get_desc_limit(): Int = conf.getInt("ontology_depth.desc_deph")

  def get_gcm_table_list(): List[String] = conf.getStringList("gcm_table_list").asScala.toList
}

package Config

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import scala.collection.JavaConverters._

object config  {

  val parsedConfig: Config = ConfigFactory.parseFile(new File("src/main_enricher/scala/Config/application.conf"))
  val conf: Config = ConfigFactory.load(parsedConfig)

  def get_threshold():Int = conf.getInt("threshold_match")

  def get_ontologies_by_type(term_type: String): List[String] = {
    val table_list = conf.getObject("db_config").keySet()
    var table = ""
    for (elem <- table_list.asScala) {
    val t = get_termtype_list(elem)
    if (t.exists(_.equals(term_type)))
      table = elem
    }
    conf.getStringList(s"db_config.$table.$term_type.ontologies").asScala.toList
  }

  def get_termtype_list(table: String): List[String] = conf.getObject(s"db_config.$table").keySet().asScala.toList

  def get_anc_limit(): Int = conf.getInt("ontology_depth.anc_depth")

  def get_desc_limit(): Int = conf.getInt("ontology_depth.desc_depth")

  def get_gcm_table_list(): List[String] = conf.getObject("db_config").keySet().asScala.toList
}

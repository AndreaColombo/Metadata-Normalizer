package Enricher.Enrichment_engine

import java.sql.BatchUpdateException

import Enricher.DBCon.{default_values, db_handler, user_feedback_type}
import Ols_interface.{ols_get_info,ols_search_term,ols_get_user_feedback,ols_get_status,ols_get_iri}

import util.control.Breaks._
import Config.config.{get_anc_limit, get_desc_limit, get_ontologies_by_type}
import org.apache.log4j._
import Utilities.Utils.get_timestamp

object annotator {
  val max_depth_anc: Int = get_anc_limit()
  val max_depth_desc: Int = get_desc_limit()

  val logger: Logger = Logger.getLogger(this.getClass)

  def search_term(raw_value: String, term_type: String): (String, String) = {
    var res: List[(String, String, String, String, String, String, String, String)] = List()
    var result: (String, String) = ("","")
    val ontos = get_ontologies_by_type(term_type)
    var ok = false
    for (onto <- ontos if !ok){
      val tmp = ols_search_term(raw_value,onto)
      breakable {
        if (tmp._1 == "null")
          break()
        else {
          result = (tmp._1,tmp._2)
          ok = true
        }
      }
    }
    result
  }

  def get_info(source: String, code: String, raw_value: String,table: String, column: String): List[Map[String, String]] = {
    var result: List[Map[String, String]] = List()
    val tmp = ols_get_info(source,code)
    if (tmp.nonEmpty) {
      val onto = tmp.head.head
      val parents = tmp.head(5)
      val children = tmp.head(6)

      val desc = get_desc(children, onto, 0,tmp.head(1))
      val anc = get_hyp(parents, onto, 0,tmp.head(1))

      if(!db_handler.cv_support_exists(onto,tmp.head(1)))
      result :+= Map("source" -> onto, "code" -> tmp.head(1), "label" -> tmp.head(2), "xref" -> tmp.head(3), "syn" -> tmp.head(4), "parents" -> tmp.head(5), "part_of" -> tmp.head(7),"description"->tmp.head(8),"iri"->tmp.head.last)

      //IN DESC CI SONO I DISCENDENTI DEL CURRENT TERM
      //IN ANC I SONO GLI ANCESTORS DEL CURRENT TERM

      for (elem <- anc) {
        if(!db_handler.cv_support_exists(elem._1,elem._2))
        result :+= Map("source" -> elem._1, "code" -> elem._2, "label" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8,"description"->elem._9,"iri"->elem._10)
      }

      for (elem <- desc) {
        if (!db_handler.cv_support_exists(elem._1, elem._2))
        result :+= Map("source" -> elem._1, "code" -> elem._2, "label" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8,"description"->elem._9,"iri"->elem._10)
      }
    }
    else {
      db_handler.user_feedback_insert(List(user_feedback_type(-1,resolved = false,table,column,null,raw_value,null,null,Some(source),Some(code),null,"ONLINE:ERROR  "+ols_get_status(source,ols_get_iri(source,code)),get_timestamp())))
      logger.info(s"Value $raw_value, best match found as $source $code but online resource not available")
    }
    result.distinct
  }

  def get_user_feedback(value: String,table_name: String, term_type: String): Unit = {
    var user_feedback: List[user_feedback_type] = List()
    user_feedback = ols_get_user_feedback(value, term_type, table_name)
    if (user_feedback.nonEmpty) {
      try {
        db_handler.user_feedback_insert(user_feedback)
        logger.info(s"Value $value, best match not found in online KB, user feedback")
      }
      catch {
        case e: BatchUpdateException => logger.info("User feedback exception",e.getNextException)
      }
    }
    else {
      db_handler.user_feedback_insert(List(user_feedback_type(default_values.int,default_values.bool,table_name, term_type, null, value, null, null, null, null,null,"ONLINE:NONE",get_timestamp())))
      logger.info(s"Value $value, best match not found in online KB, user feedback not found")
    }
  }

  def get_desc(children: String, onto: String, depth: Int, previous_code: String): List[(String, String, String, String, String, String, String, String,String,String)] = {
    var result: List[(String, String, String, String, String, String, String, String, String, String)] = List()
    for (code <- children.split(",")) {
      if (code != "null") {
        val res = ols_get_info(onto,code)
        if(res.nonEmpty) {
          result :+= (res.head.head, res.head(1), res.head(2), res.head(3), res.head(4), res.head(5), res.head(6), res.head(7), res.head(8),res.head(9))
          val n = depth + 1
          if (n != max_depth_desc)
            result ++= get_desc(res.head(6), res.head.head, n,previous_code+","+code)
          else
            result
        }
        else logger.warn(s"Error in descendant retrieval, code $code with path $previous_code")
      }
    }
    result
  }

  def get_hyp(parents: String, onto: String, depth: Int, previous_code: String): List[(String, String, String, String, String, String, String, String,String,String)] = {
    var result: List[(String, String, String, String, String, String, String, String, String, String)] = List()
    for (code <- parents.split(",")) {
      if (code != "null") {
        val res = ols_get_info(onto,code)
        if (res.nonEmpty) {
          result :+= (res.head.head, res.head(1), res.head(2), res.head(3), res.head(4), res.head(5), res.head(6), res.head(7), res.head(8),res.head(9))
          val n = depth + 1
          if (n != max_depth_anc)
            result ++= get_hyp(res.head(5), res.head.head, n,previous_code+","+code)
          else
            result
        }
        else logger.warn(s"Error in hypernym retrieval, code $code with path $previous_code")
      }
    }
    result
  }


}
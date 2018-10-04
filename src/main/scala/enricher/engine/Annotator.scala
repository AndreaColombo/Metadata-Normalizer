/*****************************************************************
E' TUTTO DA BUTTARE
package enricher.engine

import java.sql.BatchUpdateException

import config_pkg.ApplicationConfig._
import enricher.dbcon.{DbHandler, default_values, expert_choice_type}
import enricher.engine.Ols_interface._
import utilities.Utils.get_timestamp
import org.apache.log4j._

object Annotator {
  val max_depth_anc: Int = get_anc_limit()
  val max_depth_desc: Int = get_desc_limit()

  val logger: Logger = Logger.getLogger(this.getClass)

//  def search_term(raw_value: String, term_type: String): Term = {
//    var result = search_term_result(List(),0.0)
//    val ontos = get_ontologies_by_type(term_type)
//    var ok = false
//    var best_score = 0.0
//    val tmp = ols_search_term(raw_value,ontos)
//     TODO ARIF max score
//    if (tmp.score==get_match_score("pref")){
//      result = tmp
//      ok = true
//    }
//    else if (tmp.score > best_score){
//      result = tmp
//      best_score = tmp.score
//    }
//    else if (tmp.score == best_score){ //TODO ARIF add check also match_mode_random
//      val l = result.options ++ tmp.options
//      result = search_term_result(l,tmp.score)
//    }
//    result
//  }


  def get_info(ontology: String, code: String, raw_value: String,table: String, column: String): List[Map[String, String = {
    var result: List[Map[String, String = List()

    //TODO before starting this check RAW table, then check
    val tmp = ols_get_info(ontology,code)
    if (tmp.nonEmpty) {
      val onto = tmp.head.head
      val parents = tmp.head(5)
      val children = tmp.head(6)

      if(!DbHandler.cv_support_exists(onto,tmp.head(1)))
        result :+= Map("ontology" -> onto, "code" -> tmp.head(1), "iri" -> tmp.head(2), "xref" -> tmp.head(3), "syn" -> tmp.head(4), "parents" -> tmp.head(5), "part_of" -> tmp.head(7),"description"->tmp.head(8),"iri"->tmp.head.last)

      //IN DESC CI SONO I DISCENDENTI DEL CURRENT TERM
      //IN ANC I SONO GLI ANCESTORS DEL CURRENT TERM

// TODO ARIF activate hyp
      //      val desc = get_desc(children, onto, 0,tmp.head(1))
      //      val anc = get_hyp(parents, onto, 0,tmp.head(1))
//      for (elem <- anc) {
//        if(!DbHandler.cv_support_exists(elem._1,elem._2))
//        result :+= Map("ontology" -> elem._1, "code" -> elem._2, "iri" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8,"description"->elem._9,"iri"->elem._10)
//      }
//
//      for (elem <- desc) {
//        if (!DbHandler.cv_support_exists(elem._1, elem._2))
//        result :+= Map("ontology" -> elem._1, "code" -> elem._2, "iri" -> elem._3, "xref" -> elem._4, "syn" -> elem._5, "parents" -> elem._6, "part_of" -> elem._8,"description"->elem._9,"iri"->elem._10)
//      }
    }
    else {
      DbHandler.user_feedback_insert(List(expert_choice_type(-1,resolved = false,table,column,null,raw_value,null,null,Some(ontology),Some(code),null,"ONLINE:ERROR  "+ols_get_status(ontology,ols_get_iri(ontology,code)),get_timestamp())))
      logger.info(s"Value $raw_value, best match found as $ontology $code but online resource not available")
    }
    result.distinct
  }

  def get_user_feedback(value: String,table_name: String, term_type: String): Unit = {
    var user_feedback: List[expert_choice_type] = List()
    user_feedback = ols_get_user_feedback(value, term_type, table_name)
    if (user_feedback.nonEmpty) {
      try {
        DbHandler.user_feedback_insert(user_feedback)
        logger.info(s"Value $value, best match not found in online KB, user feedback")
      }
      catch {
        case e: BatchUpdateException => logger.info("User feedback exception",e.getNextException)
      }
    }
    else {
      DbHandler.user_feedback_insert(List(expert_choice_type(default_values.int,default_values.bool,table_name, term_type, null, value, null, null, null, null,null,"ONLINE:NONE",get_timestamp())))
      logger.info(s"Value $value, best match not found in online KB, user feedback not found")
    }
  }

  /*
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
*/

}

*******************************************************************/
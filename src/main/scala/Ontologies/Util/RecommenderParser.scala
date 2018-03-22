package Ontologies.Util

import play.api.libs.json._
import scala.collection.mutable.ListBuffer
import java.lang.Float.parseFloat


object RecommenderParser {

  def parse (s: String): List[List[String]] = {
    val j = Json.parse(s)
    val scores = j \\ "evaluationScore"
    val ontologies = j \\ "ontologies"
    val terms = j \\ "coverageResult"
    val matchType = j \\ "matchType"

    var rows = ListBuffer[List[String]]()

    for (i <- ontologies.indices) {
      val service = "Recommender Keywords"
      var row = ListBuffer[String]()
      val l_ontologies = foo(ontologies(i), "acronym")
      val l_terms = foo(terms(i), "text")
      val terms_n = l_terms.length
      val position = i + 1
      val score = scores(i)
      var lst = ""
      l_terms.foreach(lst += _ + "; ")
      row.append(position.toString,lst.map(_.toLower).replace(""""""","").dropRight(2), (parseFloat(score.toString())*100).toString)
      lst = ""
      l_ontologies.foreach(lst += _ + " ")
      val l = lst.map(_.toLower).replace(""""""","")
      row.append(lst.map(_.toLower).replace(""""""","").dropRight(1))
      row.append(terms_n.toString)
      rows.append(row.toList)
    }

    rows.toList
  }

  private def foo(s: JsValue, term: String): List[String] = {
    val set = s \\ term
    if (term.equalsIgnoreCase("acronym"))
      f(set.map(_.toString()).toList)

    set.map(_.toString()).toList
  }
  private def f(arr: List[String]): List[String] = arr.indices.collect { case i if i % 2 == 0 => arr(i) }.toList


}
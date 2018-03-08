package Ontologies.Util

import play.api.libs.json._
import scala.collection.mutable.ListBuffer


object RecommenderParser {

  def parse (s: String): List[List[String]] = {
    val j = Json.parse(s)
    val scores = j \\ "evaluationScore"
    val ontologies = j \\ "ontologies"
    val terms = j \\ "coverageResult"

    var rows = ListBuffer[List[String]]()

    for (i <- ontologies.indices) {
      var row = ListBuffer[String]()
      val l_ontologies = foo(ontologies(i), "acronym")
      val l_terms = foo(terms(i), "text")
      val position = i + 1
      val score = scores(i)
      var lst = ""
      l_terms.foreach(lst += _ + " ")
      row.append(lst, score.toString(), position.toString)
      lst = ""
      l_ontologies.foreach(lst += _ + " ")
      row.append(lst)
      rows.append(row.toList)
    }

    return rows.toList
  }

  def foo(s: JsValue, term: String): List[String] = {
    val set = s \\ term
    if (term.equalsIgnoreCase("acronym"))
      return (f(set.map(_.toString()).toList))

    return (set.map(_.toString()).toList)
  }

  def f(arr: List[String]): List[String] = (arr.indices.collect { case i if i % 2 == 0 => arr(i) }).toList

}
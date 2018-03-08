import BioPortal.Requests._
import play.api.libs.json.{JsValue, Json}

import Ontologies.Ontology
import scala.collection.mutable.ListBuffer

object main extends App {
  val path = "C:/Users/Andrea Colombo/IdeaProjects/Tesi/"

  val input = "leukocyte,phagocyte,motile cell,metabolising cell,dendritic cell,pipette"

  val recommender = Ontology.apply("recommender")

  println(recommender.get_results(input))


}
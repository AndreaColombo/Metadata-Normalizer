import java.io.{File, FileWriter, PrintWriter}

import DBcon.db_handler
import com.github.tototoshi.csv.CSVWriter
import play.api.libs.json.Json
import scalaj.http.Http

object report_writer {
  val m = Map("biosample" -> List("disease", "tissue", "cell_line"), "donor" -> List("ethnicity", "species"), "item" -> List("platform"), "experiment_type" -> List("technique","target","feature"), "container" -> List("annotation"))

  def write_report(t: String): Unit = {
    val ttl = m.apply(t)
    val apikey = "2338fb64-0246-4627-bf4d-4197bc8c9c64"
    val s = """\paragraph{}
              |\begin{itemize}
              |\end{itemize}
              |
              |\begin{itemize}
              |    \item Coverage:
              |    \item Score:
              |    \item Suitability:
              |    \item Categories:
              |\end{itemize}"""
    var r = ""
    val url = "http://data.bioontology.org/ontologies/"
    for (tt <- ttl) {
      println(tt)
      r += """\subsection{""" + t + ":" + tt.replace("_"," ")+"""}""" + "\n"
      r += """\subsubsection{Best ontologies}""" + "\n"
      //BEST ONTOS
      val ontos = db_handler.get_best_onto_per_term(tt)
      for (i <- 0 until 3) {
        val onto = ontos(i)._2
        val scores = db_handler.get_score_suitability(onto, tt)
        val coverage = ontos(i)._5
        val score = scores._1
        val suit = scores._3
        r += """\paragraph{""" + onto +"""}""" + "\n"
        val onto_data = Json.parse(Http(url + onto.map(_.toUpper)).param("display_context", "false").param("apikey", apikey).header("accept", "text/json").asString.body)
        val name = (onto_data \ "name").validate[String].get
        r += name + "\n"
        val url2 = (onto_data \ "links" \ "categories").validate[String].get
        val categories = (Json.parse(Http(url2).param("apikey", apikey).header("accept", "text/json").asString.body) \\ "name")

        r +=
          """\begin{itemize}""" + "\n" +
            """\item Coverage: """ + coverage + "\n" +
            """\item Score: """ + score.toString + "\n" +
            """\item Suitability: """ + suit.toString + "\n" +
            """\item Categories: """ + categories.mkString(", ").replaceAll(""""""", "") + "\n" +
            """\end{itemize}""" + "\n"
      }

      r += "\n" +"""\subsubsection{Best ontologies sets}""" + "\n"
      val onto_sets = db_handler.get_best_ontos_per_term(tt)
      var i = 0
      for (os <- onto_sets if i < 3) {
        val onto = os._1
        val score = os._2
        val coverage = os._3
        val suit = os._4
        r += """\paragraph{""" + onto +"""}""" + "\n"
        val ontos = onto.split(",")
        var categories = ""
        r += """\begin{itemize}""" + "\n"
        for (o <- ontos) {
          val onto_data = Json.parse(Http(url + o.map(_.toUpper)).param("display_context", "false").param("apikey", apikey).header("accept", "text/json").asString.body)
          val name = (onto_data \ "name").validate[String].get
          r += """\item """ + o.map(_.toUpper) + "," + name + "\n"
          val url2 = (onto_data \ "links" \ "categories").validate[String].get
          val a = (Json.parse(Http(url2).param("apikey", apikey).header("accept", "text/json").asString.body) \\ "name").mkString(",").replaceAll(""""""", "")
          if (a.nonEmpty)
            categories += a + "(" + o.map(_.toUpper) + ")" + ", "
        }
        r += """\end{itemize}""" + "\n"

        r +=
          """\begin{itemize}""" + "\n" +
            """\item Coverage: """ + coverage + "\n" +
            """\item Score: """ + score.toString + "\n" +
            """\item Suitability: """ + suit.toString + "\n" +
            """\item Categories: """ + categories.dropRight(2) + "\n" +
            """\end{itemize}""" + "\n"
        r += "\n"
        i += 1
      }
    }
    val f = new File("report_final_" + t.replace("_","-") + ".csv")
    val writer = new PrintWriter(f)
    writer.write(r)
    writer.flush()


  }

}

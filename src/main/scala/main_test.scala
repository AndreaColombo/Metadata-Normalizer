import Config.config
import Enricher.DBCon.db_handler

object main_test {

  def main(args: Array[String]): Unit = {
    val term = "a b c d"
    val label = "b c e"
    val term_set = term.split("[ ,!.\\-/]+").toSet
    val label_set = label.split("[ ,!.\\-/]+").toSet
    println(term.replaceAll("[()\\[\\]{}]",""))
    val in_common = term_set.intersect(label_set)
    val excess_t = term_set -- in_common
    val excess_l = label_set -- in_common
    println("Word 1: "+term)
    println("Word 2: "+label)
    println("Common words: "+in_common)
    println("Diff t: "+excess_t)
    println("Diff l: "+excess_l)

    val deletion = excess_t
    val insertion = excess_l
    val diff = deletion.size * config.get_modifier("deletion") + insertion.size * config.get_modifier("insertion")
    println(diff)

  }
}

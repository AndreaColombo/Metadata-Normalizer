package utilities

import java.io._

import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

/**
  * This object contains various utilities methods
  */
object Utils {

  /**
    * Return system current time
    * @return
    */
  def get_timestamp(): String = {
    DateTime.now.toString(DateTimeFormat.forPattern("yyyy_MM_dd_HH_mm_ss_SSS"))
  }

  /**
    * Eliminates duplicates from list based on condition
    */
  def distinctBy[L, E](list: List[L])(f: L => E): List[L] =
    list.foldLeft((Vector.empty[L], Set.empty[E])) {
      case ((acc, set), item) =>
        val key = f(item)
        if (set.contains(key)) (acc, set)
        else (acc :+ item, set + key)
    }._1.toList
}

package user_interface

import Enricher.DBCon.db_handler
import Utils._
import scalax.cli.Table
import shapeless.Sized

import scala.io.StdIn

object Expert_feedback {

  def get_user_rating(): Unit = {
    println("Choose User")
    val user_l = db_handler.get_username_list()
    for (i <- user_l.indices){
      println(i+1+" - "+user_l(i))
    }
    var username = ""
    println(user_l.length+1 + " - New User")
    val choice = get_choice(user_l.length+1)
    if(choice.toInt==user_l.length+1){
      println("Input User Name")
      username = StdIn.readLine()
    }
    else username = user_l(choice.toInt-1)

    val tmp = choose_table_column()
    val table_name = tmp._1
    val column = tmp._2

    val info_for_rating = db_handler.get_info_for_feedback(table_name,column)

    println("You are currently rating annotations for")
    println("Table: "+table_name)
    println("Column: "+column)
    val table = Table(Sized("id","parsed value","label","source","code","iri"))

    for (o <- info_for_rating){
      table.rows += Sized(o.raw,o.pref_label,o.source,o.code,o.iri,o.description)
      table.print()
    }

  }

}

package user_interface

import Enricher.DBCon.{db_handler, expert_feedback_type}
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

    val info_for_rating = db_handler.get_info_for_feedback(table_name,column,username)

    println("Hello "+username)
    println("You are currently rating annotations for")
    println("Table: "+table_name)
    println("Column: "+column)
    println
    println

    for (o <- info_for_rating){
      val table = Table(Sized("Raw value","Preferred label","Source","Code","Iri","Description"))
      table.rows += Sized(o.raw,o.pref_label,o.source,o.code,o.iri,o.description)
      table.print()
      println
      println("Rate this annotation")
      println("1 - EXACT")
      println("2 - ALMOST EXACT")
      println("3 - ACCEPTABLE")
      println("4 - BAD")
      val rating = get_choice(4).toInt
      val row = expert_feedback_type(username,o.raw,table_name,column,o.tid,rating)
      db_handler.insert_expert_feedback(List(row))
      if (rating == 4){
        println("Do you want to correct this annotation?")
        println("1 - Yes")
        println("2 - No")
        val choice = get_choice(2).toInt
        if(choice==1){
          Expert_correction.correct_value(o.raw,table_name,column)
        }
      }
    }

  }

}

package Recommender.DBCon

import Config.config.conf
import Recommender.DBCon.Tables.{apiresults, ontologyScore, best_onto_set}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration.Duration

object db_handler {

  private val db = Database.forConfig("gecotest2", conf)

  val setup2 = ontologyScore.schema.create
  val setup3  = best_onto_set.schema.create
  val setupapiresults2 = apiresults.schema.create

  db.run(setup2)
  db.run(setup3)
  db.run(setupapiresults2)

  def apiresults_insert(rows: List[List[String]], term_type: String): Unit = {
    var ok: Seq[(String, String, String, String, String, String, String, String, String)] = Seq()

    for (l <- rows) {
      ok :+= (l.head, l(1), l(2), l(3), l(4), l(5), l(6), l(7), term_type)
    }
    val db = Database.forConfig("gecotest2", conf)
    val insertAction = apiresults.map(a=> (a.service,a.raw_value,a.parsed_value,a.ontology,a.ontology_id,a.pref_label,a.synonym,a.score,a.term_type)) ++= ok
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
    println("ok apiresults_insert")
  }

  def update_raw_value(rawValue: String, parsed: String): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sqlu"""update public.apiresults
             set raw_value = $rawValue
             where parsed_value = $parsed
          """

    val result_future = db.run(q)
    Await.result(result_future, Duration.Inf)
    db.close()
  }

  def insert_best_ontos(rows: Iterable[(String, String, Double, Double, Double)]): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val insertAction = best_onto_set ++= rows
    val insert = db.run(insertAction)
    Await.result(insert, Duration.Inf)
    db.close()
  }

  def update_score(score1: Double, score2: Double, ontoScore: Double, matchScore: Int, id: Int): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sqlu"""
             update public.apiresults
             set score_num1 = $score1,
             score_num2 = $score2,
             onto_score = $ontoScore,
             match_score = $matchScore
             where id = $id
        """

    val result_future = db.run(q)
    Await.result(result_future, Duration.Inf)
    db.close()
  }

  def update_suitability(suitability: Double, ontology: String, term_type: String): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sqlu"""
             update public.apiresults
             set suitability = $suitability
             where ontology ilike $ontology and term_type ilike $term_type
        """

    val result_future = db.run(q)
    Await.result(result_future, Duration.Inf)
    db.close()
  }

  def update_suitability_sets(suitability: Double, ontology: String, term_type: String): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sqlu"""
             update public.best_onto_sets
             set set_suitability = $suitability
             where ontologies_set ilike $ontology and term_type ilike $term_type
        """

    val result_future = db.run(q)
    Await.result(result_future, Duration.Inf)
    db.close()
  }

  def update_term_type(parsedValue: String, term_type: String): Unit = {
    val q =
      sqlu"""update public.apiresults
            set term_type = $term_type
         where parsed_value ilike $parsedValue
          """
    val db = Database.forConfig("gecotest2", conf)
    Await.result(db.run(q), Duration.Inf)
    db.close()
  }

  def get_match_type(id: Int, service: String): String = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select score
           from public.apiresults
           where id = $id
           and service ilike $service
         """.as[String]

    var match_type = ""
    val future_match = db.run(q).map(a => match_type = a.head)

    Await.result(future_match, Duration.Inf)
    db.close()
    match_type
  }

  def ontology_score_insert(rows: Seq[(String, Double)]): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val insertaction = ontologyScore ++= rows
    val result_future = db.run(insertaction)
    Await.result(result_future, Duration.Inf)
    db.close()
  }

  def get_ontology_by_type(term_type: String): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("gecotest2", conf)

    val q =
      sql"""
           select distinct ontology
           from public.apiresults
           where term_type ilike $term_type
         """.as[String]
    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))
    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }

  def get_ontologies(): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("gecotest2", conf)

    val q =
      sql"""
           select distinct ontology
           from public.apiresults
         """.as[String]
    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))
    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }

  def get_term_by_ontology(ontology: String, term_type: String): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("gecotest2", conf)

    val q =
      sql"""
           select distinct raw_value
           from public.apiresults
           where term_type ilike $term_type and ontology ilike $ontology
         """.as[String]

    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))

    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }

  def get_term_type(term: String): String = {
    var result = ""
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select term_type
           from public.apiresults
           where raw_value ilike $term
         """.as[String]

    val result_future = db.run(q).map(
      a => result = a.head
    )

    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_term_by_type(term_type: String): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("gecotest2", conf)

    val q =
      sql"""
           select distinct raw_value
           from public.apiresults
           where term_type ilike $term_type
         """.as[String]

    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))

    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }

  def get_parsed_by_ontology(ontology: String): List[String] = {
    var result: Seq[String] = List()
    val db = Database.forConfig("gecotest2", conf)

    val q =
      sql"""
           select distinct parsed_value
           from public.apiresults
           where ontology ilike $ontology
         """.as[String]

    val result_future = db.run(q).map(_.foreach(
      a => result :+= a
    ))

    Await.result(result_future, Duration.Inf)
    db.close()
    result.toList
  }

  def get_onto_score(onto: String, term_type: String): String = {
    val db = Database.forConfig("gecotest2", conf)
    var score = "0"
    val q =
      sql"""
           select score
           from public.ontologyscore
           where ontology ilike $onto
         """.as[String]
    val result_future = db.run(q).map(a =>
    if(a.nonEmpty)
      score = a.head
    )
    Await.result(result_future, Duration.Inf)
    db.close()
    score
  }

  def get_db_lenght(): List[Int] = {
    val db = Database.forConfig("gecotest2", conf)
    var lenght: List[Int] = List()
    val q =
      sql"""
           select id
           from public.apiresults
         """.as[Int]

    val result_future = db.run(q).map(a => lenght = a.toList)
    Await.result(result_future, Duration.Inf)
    db.close()
    lenght
  }

  def get_onto_service_termtype(id: Int): (String, String, String) = {
    var result = ("", "", "")
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select ontology, service, term_type
           from public.apiresults
           where id = $id
         """.as[(String, String, String)]

    val result_future = db.run(q).map(a =>
      result = a.head
    )
    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_onto_ontoid(id: Int): (String, String) = {
    var result = ("", "")
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select ontology, ontology_id
           from public.apiresults
           where id = $id
         """.as[(String, String)]

    val result_future = db.run(q).map(a =>
      result = a.head
    )
    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def delete_row(id: Int): Unit = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sqlu"""
            update public.apiresults
            set deleted = "true"
            where id = $id
          """
    val resultFuture = db.run(q)
    Await.result(resultFuture, Duration.Inf)
    db.close()
  }

  def get_onto_coverage(onto: String, term_type: String): (String, String) = {
    var result = ("", "")
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select count(distinct(raw_value)) / (select count(distinct(raw_value)) from public.apiresults where term_type ilike $term_type)::Float, count(distinct(raw_value))
           from public.apiresults
           where ontology ilike $onto and term_type ilike $term_type
         """.as[(String, String)]

    val result_future = db.run(q).map(a =>
      result = a.head
    )
    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_onto_matchscore(onto: String, term_type: String): String = {
    var result = ""
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select sum(match_score)
           from public.apiresults
           where ontology ilike $onto and term_type ilike $term_type
         """.as[String]

    val result_future = db.run(q).map(a =>
      result = a.head
    )
    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_best_onto_per_term(term_type: String): Seq[(String, String, String, String, String)] = {
    var result = Seq(("", "", "", "", ""))
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select *
           from public.best_onto_per_term
           where term_type = $term_type
         """.as[(String, String, String, String, String)]

    val result_future = db.run(q).map(a=>
      result = a
    )

    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_best_ontos_per_term(term_type: String): Seq[(String, String, String, String,String)] = {
    var result = Seq(("", "", "", "",""))
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select ontologies_set, set_score1, set_coverage, set_suitability, (char_length(ontologies_set)-char_length(replace(ontologies_set,',',''))+1) as num_ontos
           from public.best_onto_sets
           where term_type ilike $term_type
           order by term_type, set_coverage desc, num_ontos asc, set_suitability desc
         """.as[(String, String, String, String, String)]

    val result_future = db.run(q).map(a=>
      result = a
    )

    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }


  def get_nrv(term_type: String): Int = {
    var result = 0
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select nrv_count
           from public.num_raw_values
           where term_type = $term_type
         """.as[Int]

    val result_future = db.run(q).map(a=>
      result = a.head
    )

    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_score_suitability(onto: String, term_type: String): (Double, Double) = {
    var result: (Double, Double) = (0.0,0.0)
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
           select avg_score1, suitability
           from public.best_onto_per_term
           where term_type ilike $term_type and ontology ilike $onto
         """.as[(Double, Double)]
    val result_future = db.run(q).map(a=>
      result = a.head
    )


    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_max_score(rv: String, onto:String): String = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
            select match_score
           	from public.apiresults
           	where match_score = (select max(match_score) from public.apiresults where replace(replace(raw_value,'"',''),'\','') ilike $rv and ontology ilike $onto)
           	and replace(replace(raw_value,'"',''),'\','') ilike $rv and ontology ilike $onto
           	order by service
            limit 1
         """.as[String]
    var result = ""
    val result_future = db.run(q).map(a => result = a.head)

    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }

  def get_onto_sets(t: String): List[String] = {
    val db = Database.forConfig("gecotest2", conf)
    val q =
      sql"""
            select ontologies_set
           	from public.best_onto_sets
           	where term_type ilike $t
         """.as[String]
    var result = List("")

    val result_future = db.run(q).map(a => result = a.toList)

    Await.result(result_future, Duration.Inf)
    db.close()
    result
  }
  def get_raw_values(table: String, term_type: String): List[String] = {
    val db = Database.forConfig("gecotest2", conf)
    var result: Seq[String] = List()
    val t = term_type
    val q =
      sql"""select distinct #$t as value
           from #$table
           where #$t IS NOT NULL
         """
    try {
      val result_future = db.run(q.as[String]).map(_.foreach(a =>
        result :+= a))
      Await.result(result_future, Duration.Inf)
    }
    finally db.close()
    result.toList
  }

  def create_view(): Unit = {
//    val q0 = sqlu"""
//                DROP VIEW support_table;
//                DROP VIEW num_raw_values;
//                DROP VIEW best_onto_per_term2"""

    val q1 = sqlu"""
                create view support_table as
                    SELECT term_type, raw_value, max(score_num1) as score1, lower(ontology) as ontology, array_agg(ontology_id), max(suitability) as suitability
                    FROM apiresults
                    GROUP BY term_type, raw_value, lower(ontology)
                    ORDER BY term_type, raw_value, score1 desc, suitability desc, lower(ontology)"""

    val q2 = sqlu"""
                create view num_raw_values as
                   	SELECT term_type, count(distinct raw_value) as nrv_count
                   	from apiresults
                   	Group by term_type"""
    val q3 = sqlu"""
                create view best_onto_per_term as
                    SELECT term_type, ontology, avg(score1) as avg_score1, count(*)/max(nrv_count)::float as coverage, suitability
                    FROM support_table  NATURAL JOIN num_raw_values
                    GROUP BY term_type,ontology, suitability
                    order by coverage desc, avg_score1 desc, suitability desc, term_type"""
    val createAction = DBIO.seq(q1,q2,q3)
    val db = Database.forConfig("gecotest2",conf)
    Await.result(db.run(createAction),Duration.Inf)
    db.close()
  }

  def get_recsys(service: String): List[List[String]] = {
    val q = apiresults.filter(a => a.service === service).map(a => (a.service,a.term_type,a.raw_value,a.parsed_value,a.ontology,a.ontology_id,a.pref_label,a.synonym,a.score))
    var result: List[List[String]] = List()
    val db = Database.forConfig("gecotest2",conf)
    val f = db.run(q.result).map(_.foreach(
      a => result:+= List(a._1,a._2,a._3,a._4,a._5.map(_.toLower),a._6,a._7,a._8,a._9)
    ))
    Await.result(f,Duration.Inf)
    db.close()
    result
  }
}
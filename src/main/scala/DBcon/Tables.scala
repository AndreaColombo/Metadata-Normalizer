package DBcon

import slick.jdbc.PostgresProfile.api._

object Tables {

  class t1(tag: Tag) extends Table[(String, String, String)](tag, Some("svr"),"t1") {
    def s_id = column[String]("s_id", O.PrimaryKey)
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (s_id, key, value)
  }
  val t1 = TableQuery[t1]

  class t2(tag: Tag) extends Table[(String, String, String)](tag, "t2") {
    def s_id = column[String]("s_id")
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (s_id, key, value)
  }

  class prova(tag: Tag) extends Table[(String, String, String)](tag, Some("svr"), "prova") {
    def s_id = column[String]("s_id")
    def key = column[String]("key")
    def value = column[String]("value")
    def * = (s_id, key, value)
  }
}

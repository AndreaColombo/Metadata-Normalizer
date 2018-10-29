import java.net.HttpCookie

import config_pkg.ApplicationConfig._
import enricher.engine.OlsInterface._
import enricher.engine.{OlsInterface, RawValue, ScoredTerm, Term}
import scalaj.http.{Http, HttpOptions}
import utilities.Utils

import sys.process._

object main_test {

  def main(args: Array[String]): Unit = {
    //    val term = Term(OlsInterface.ols_get_onto_info("uberon"),"","http://purl.obolibrary.org/obo/UBERON_0002107")

    //    val tmp = term.fill().saveToKB().fill_relation()
    //    tmp.save_relation()
    while(true) {
      val ck = "eyJpdiI6ImgzQTQ2dDNpK0llTFl2eVwvQTlnc29CMFVGQnJNQVVBMnd4eXA3Y0ZRV2lVPSIsInZhbHVlIjoiRG80SkN0MWtQYk84ZkNDVEM1TjBjbDJ1Sko2RWMyT1NrZldrZkc3aTRvZzFEZ0huaGIyTDZmcllkVk9ZcEpyeGRleDdYWWQ4ckVhVFROTkFDVGFzdFE9PSIsIm1hYyI6IjNlYjA0MWJhYWExNTRhODQ1M2UxNjRhNTU3ZmQwYTc1NTg1ODc4YzY4MzYyZWJlZmU2Mzc4N2RhZGIyNWM1MzMifQ"
      val a = Http("https://www.finchvpn.com/pptp").cookie("finchvpn_session", ck).auth("archand8@gmail.com", "moebius8156").option(HttpOptions.connTimeout(10000)).option(HttpOptions.readTimeout(50000)).asString

      val psw = a.body.substring(5766, 5766 + "k2MhDPLjTF".length)
      println(psw)
      val cmd = "rasdial finch finchvpn "+psw
      val v = cmd.!
      println(Utils.get_timestamp())
      if (v == 0) {
        Thread.sleep(2 * 60 * 1000)
      }
    }
  }
}

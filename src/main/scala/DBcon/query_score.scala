package DBcon

import java.io.File

import com.typesafe.config.ConfigFactory

object query_score {
  private val parsedConfig = ConfigFactory.parseFile(new File("src/main/scala/DBcon/application.conf"))
  private val conf= ConfigFactory.load(parsedConfig)


}

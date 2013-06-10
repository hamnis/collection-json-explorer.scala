package io.trygvis.cj

import java.net.{HttpURLConnection, URI}
import java.io.{FileReader, File}
import org.json4s.native.JsonParser
import org.json4s.JsonAST._
import scala.util.Properties
import org.apache.commons.codec.binary.Base64

object Config {
  val auth: List[AuthConfig] = Properties.propOrNone("auth-config") match {
    case Some(f) => load(new File(f))
    case None => Nil
  }

  private def load(file: File): List[AuthConfig] = {
    if (file.exists()) parse(JsonParser.parse(new FileReader(file), closeAutomatically = true)) else Nil
  }

  private def parse(value: JValue): List[AuthConfig] = {
    value match {
      case JObject(List(JField("auth", JArray(arr)))) => parse(arr)
      case _ => Nil
    }
  }
  private def parse(list: List[JValue]): List[AuthConfig] = {
    list.collect{
      case obj@JObject(fields) => {
        val m = fields.foldLeft(Map.empty[String, JValue]){case (map, JField(n, v)) => map.updated(n, v)}
        val scheme = m.get("scheme").map(_.values.toString)
        val JString(host) = m("host")
        val JInt(port) = m("port")
        val JString(username) = m("username")
        val JString(password) = m("password")
        AuthConfig(HostConfig(scheme, host, port.toInt), username, password)
      }
    }
  }
}

case class AuthConfig(hostConfig: HostConfig, username: String, password: String) {
  def matches(url: java.net.URL) = hostConfig.matches(url.toURI)

  def apply(con: HttpURLConnection) = {
    con.addRequestProperty("Authorization", "Basic " + base64(username, password))
  }

  def base64(username: String, password: String): String = Base64.encodeBase64String(s"$username:$password".getBytes("UTF-8")).trim

}

case class HostConfig(scheme: Option[String], host: String, port: Int) {
  def matches(uri: URI) = {
    val other = HostConfig(uri)
    other match {
      case HostConfig(`scheme`, `host`, `port`) => true
      case _ => false
    }
  }
}

object HostConfig {
  def apply(uri: URI): HostConfig = HostConfig(Option(uri.getScheme), uri.getHost, uri.getPort)
}
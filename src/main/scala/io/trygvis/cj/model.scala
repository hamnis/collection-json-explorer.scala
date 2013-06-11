package io.trygvis.cj

import java.io.Reader
import org.json4s.native.JsonParser
import org.json4s._
import scala.util.control.Exception._

import Json4sHelpers._
import Collection._
import java.net._
import uritemplate.URITemplate

case class Link(underlying: JObject) {
  val href: URI = getAsString(underlying, "href").flatMap(createUri).getOrElse(throw new MissingRequiredField("link", "href"))
  val rel = getAsString(underlying, "rel").getOrElse(throw new MissingRequiredField("link", "href"))
  val name = getAsString(underlying, "name")
  val render = getAsString(underlying, "render")
  val prompt = getAsString(underlying, "prompt")
}

case class Data(underlying: JObject) {
  val name = getAsString(underlying, "name").getOrElse(throw new MissingRequiredField("data", "name"))
  val value = getAsString(underlying, "value")
  val prompt = getAsString(underlying, "prompt")
}

case class Item(underlying: JObject) {
  val href = getAsString(underlying, "href").flatMap(createUri)
  val links = getAsObjectList(underlying, "links").map(Link(_))
  val data = getAsObjectList(underlying, "data").map(Data(_))
}

case class Query(underlying: JObject) {
  import Encoding._
  val encoding = getAsString(underlying, "encoding").map{ case "uri-template" => UriTemplate; case _ => UrlEncoded}.getOrElse(UrlEncoded)
  val href = getAsString(underlying, "href").flatMap{ s =>
    encoding match {
      case UriTemplate => Some(URITemplateExpandable(s))
      case UrlEncoded => createUri(s).map(h => URIExpandable(h))
    }
  }.getOrElse(throw new MissingRequiredField("query", "href"))
  val rel = getAsString(underlying, "rel").getOrElse(throw new MissingRequiredField("query", "rel"))
  val name = getAsString(underlying, "name")
  val prompt = getAsString(underlying, "prompt")
  val data = getAsObjectList(underlying, "data").map(Data(_))
}

case class Template(underlying: JObject) {
  val data = getAsObjectList(underlying, "data").map(Data(_))

  def writeTo(os: java.io.OutputStream) {
    import org.json4s.native.JsonMethods._
    os.write(compact(render(underlying)).getBytes("UTF-8"))
  }
}

object Template extends ((JObject) => Template) {
  def apply(params: Map[String, String]): Template = {
    val arr = JArray(params.foldLeft(List.empty[JValue]){case (list, (k,v)) => List(JObject(JField("name", JString(k)), JField("value", JString(v)))) ++ list })
    new Template(JObject(JField("data", arr)))
  }
}

case class Error(underlying: JObject) {
  val title = getAsString(underlying, "title")
  val code = getAsString(underlying, "code")
  val message = getAsString(underlying, "message")
}

case class Collection(underlying: JObject) {
  val version = getAsString(underlying, "version").getOrElse("1.0")
  val href = getAsString(underlying, "href").flatMap(createUri)
  val links = getAsObjectList(underlying, "links") map { Link(_) }
  val items = getAsObjectList(underlying, "items") map { Item(_) }
  val queries = getAsObjectList(underlying, "queries") map { Query(_) }
  val template = getAsObject(underlying, "template") map { Template(_) }
  val error = getAsObject(underlying, "error") map { Error(_) }

  def findLinkByRel(rel: String) = links.find(_.rel == rel)
}

object Collection {
  def createUri(s: String): Option[URI] = allCatch.opt(URI.create(s))

  def parseCollection(reader: Reader): Either[Throwable, Collection] = {
    try {
      val parsed = JsonParser.parse(reader, closeAutomatically = true)
      parsed match {
        case JObject(List(JField("collection", x@JObject(_)))) => { allCatch.either(Collection(x))}
        case _ => throw new IllegalArgumentException("Unexpected json here. was\n %s".format(parsed))
      }
    }
    catch {
      case e : Throwable => Left(e)
    }
  }
}

sealed trait Encoding

object Encoding {
  case object UriTemplate extends Encoding
  case object UrlEncoded extends Encoding
}


sealed trait Expandable {
  def expand(): URI = expand(Map.empty)
  def expand(data: Map[String, String]): URI
}

case class URIExpandable(uri: URI) extends Expandable {
  def expand(data: Map[String, String]): URI = {
    val q = data map {
      case (key, value) =>
        URLEncoder.encode(key, "utf-8") + "=" + URLEncoder.encode(value, "utf-8")
    }

    val concat = if (uri.getQuery == null) "?" else "&"
    val collectionUrlString = uri.toString + (if (q.nonEmpty) concat + q.reduce(_ ++ "&" ++ _) else "")
    URI.create(collectionUrlString)
  }
  override val toString = uri.toString
}

case class URITemplateExpandable(template: String) extends Expandable {
  import uritemplate.Syntax._
  def expand(data: Map[String, String]): URI = URI.create(URITemplate(template).expand(data.map{case (k,v) => k := v}))
  override val toString = template
}


class MissingRequiredField(val parent: String, val field: String) extends Exception(s"Missing required field $field in $parent")

package io.trygvis.cj

import java.io.Reader
import org.json4s.native.JsonParser
import org.json4s._
import scala.util.control.Exception._

import Json4sHelpers._
import Collection._
import java.net._

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
  val href = getAsString(underlying, "href").flatMap(createUri).getOrElse(throw new MissingRequiredField("query", "href"))
  val rel = getAsString(underlying, "rel").getOrElse(throw new MissingRequiredField("query", "rel"))
  val name = getAsString(underlying, "name")
  val prompt = getAsString(underlying, "prompt")
  val data = getAsObjectList(underlying, "data").map(Data(_))
}

case class Template(underlying: JObject) {
  val data = getAsObjectList(underlying, "data").map(Data(_))
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

class MissingRequiredField(val parent: String, val field: String) extends Exception

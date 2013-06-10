package io.trygvis.cj

import scala.collection.JavaConverters._
import scala.util.Properties
import scala.util.control.Exception._
import scala.io.Source
import java.net.{URL, URLEncoder, HttpURLConnection, URI}
import java.io._
import javax.servlet.http.HttpServletRequest
import unfiltered.request._
import unfiltered.response._
import unfiltered.filter._
import unfiltered.jetty._

case class CjRequest(urlString: String, url: Option[URL], method: String, headers: Option[Map[String, Seq[String]]])
case class CjResponse(code: Int, status: String, headers: Map[String, Seq[String]], content: Option[String])

class Browser extends Plan {
  import unfiltered.directives._, Directives._

  def viewsX(req: HttpServletRequest) = {

    val (proto, port) = if (req.isSecure)
      ("https", if (req.getServerPort == 443) "" else ":" + req.getServerPort)
    else
      ("http", if (req.getServerPort == 80) "" else ":" + req.getServerPort)

    val uri = proto + "://" + req.getServerName + port + "/"
    new Views(uri)
  }

  def queryParam(name: String) = Directive[Any, Any, String]({
    request: HttpRequest[Any] =>
      request.parameterValues(name).headOption match {
        case Some(value) =>
          Result.Success(value)
        case None =>
          Result.Failure(BadRequest ~> ResponseString("Missing query parameter " + name + "\n"))
      }
  })

  def intent = {
    Path.Intent {
      case "/" =>
        for {
          _ <- GET
          req <- underlying[HttpServletRequest]
          views = viewsX(req)
        } yield Ok ~> Html5(views.index)
      case "/render" =>
        for {
          _ <- GET
          r <- underlying[HttpServletRequest]
          url <- queryParam("url")
          params <- QueryParams
          views = viewsX(r)
        } yield {
          val method = Option(r.getParameter("method")).filter {_.nonEmpty} getOrElse "GET"
          println("Request: " + method + " " + url)

          val collectionParams = params flatMap {
            case (key, value) if key.startsWith("param-") =>
              // It makes no sense to have duplicate query keys so just pick the first one. This is most likely the
              // same as most web framework does when given multiple query parameters.
              Some(key.substring(6), value.headOption getOrElse "")
            case _ =>
              None
          }

          val q = collectionParams map {
            case (key, value) =>
              URLEncoder.encode(key, "utf-8") + "=" + URLEncoder.encode(value, "utf-8")
          }

          val concat = if (URI.create(url).getQuery == null) "?" else "&"
          val collectionUrlString = url + (if (q.nonEmpty) concat + q.reduce(_ ++ "&" ++ _) else "")
          val collectionUrl = allCatch opt {URI.create(collectionUrlString).toURL } filter {_.getProtocol.startsWith("http")}

          val conO = collectionUrl map { u: java.net.URL =>
            val c = u.openConnection().asInstanceOf[HttpURLConnection]
            c.setRequestProperty("Accept", "application/vnd.collection+json,*/*;q=0.1")
            c.setRequestProperty("User-Agent", "Collection+json Explorer/1.0")
            c.setUseCaches(false)
            c
          }

          val response = conO map { c => allCatch either {
            c.connect()
            val headers = c.getHeaderFields.asScala.collect { case (key, value) if key != null => key.toLowerCase -> value.asScala.toSeq }.toMap
            val stream = if (c.getResponseCode >= 400) c.getErrorStream else c.getInputStream
            val content = allCatch opt Source.fromInputStream(stream, "utf-8").mkString("")

            c.disconnect()

            CjResponse(c.getResponseCode, c.getResponseMessage, headers, content)
          }}

          val requestHeaders = conO map { con =>
            con.getRequestProperties.asScala.collect { case (key, value) if !value.isEmpty && value.get(0) != null => key -> value.asScala.toSeq }.toMap
          }

          val isCj: (Map[String, Seq[String]]) => Boolean = headers => {
            headers.get("content-type").exists(_.contains("application/vnd.collection+json"))
          }

          val collection = response match {
            case Some(Right(CjResponse(_, _, headers, Some(content)))) if isCj(headers) =>
              Some(Collection.parseCollection(new StringReader(content)))
            case _ => None
          }

          Ok ~> Html5(views.data(CjRequest(collectionUrlString, collectionUrl, method, requestHeaders), response, collectionParams, collection))
        }
    }
  }
}

object Explorer extends App {
  val port = Properties.envOrElse("PORT", "1338").toInt
  println("Starting on port:" + port)

  Http(port).
    plan(new Browser).
    resources(getClass.getResource("/public/")).
    run(s => {
      s.current.getMimeTypes.addMimeMapping("collection+json", "application/vnd.collection+json")
  })
}

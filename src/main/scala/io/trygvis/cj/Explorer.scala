package io.trygvis.cj

import scala.collection.JavaConversions._
import scala.io.Source
import java.net.{URLEncoder, HttpURLConnection, URI}
import java.io._
import javax.servlet.http.HttpServletRequest
import unfiltered.request._
import unfiltered.response._
import unfiltered.filter._
import unfiltered.jetty._

case class CjResponse(code: Int, status: String, headers: java.util.Map[String, java.util.List[String]])

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

  def queryParam(name: String) = Directive[Any, Any, String]({ request: HttpRequest[Any] =>
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
          println("url=" + url)

          val targetParams = params flatMap {
            case (key, value) if key.startsWith("param-") =>
              // It makes no sense to have duplicate query keys so just pick the first one. This is most likely the
              // same as most web framework does when given multiple query parameters.
              Some(key.substring(6), value.headOption getOrElse "")
            case _ =>
              None
          }

          println("targetParam=" + targetParams)

          val q = targetParams map { case (key, value) =>
            URLEncoder.encode(key, "utf-8") + "=" + URLEncoder.encode(value, "utf-8")
          }

          println("q=" + q)

          val uri = URI.create(url + (if(q.nonEmpty) "?" + q.reduce (_ ++ "&" ++ _) else ""))
          println("uri=" + uri)

          val con = uri.toURL.openConnection().asInstanceOf[HttpURLConnection]
          con.setRequestProperty("accept", "application/vnd.collection+json")
          val content = Source.fromInputStream(con.getInputStream, "utf-8").mkString("")
          val headers = con.getHeaderFields.toMap filter {case (key, _) => key != null}
          val result = Collection.parseCollection(new StringReader(content))
          Ok ~> Html5(views.data(uri, targetParams, result, CjResponse(con.getResponseCode, con.getResponseMessage, headers)))
        }
    }
  }
}

object Explorer extends App {
  Http(8080).
    plan(new Browser).
    resources(getClass.getResource("/public/")).
    run()
}

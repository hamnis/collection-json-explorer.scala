package io.trygvis.cj

import scala.collection.JavaConversions._
import scala.io.Source
import java.net.{HttpURLConnection, URI}
import java.io.{Writer, StringWriter, PrintWriter, InputStreamReader}
import javax.servlet.http.HttpServletRequest
import net.hamnaberg.json.collection.{NativeJsonCollectionParser, JsonCollection}
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
          val uri = URI.create(url)
          val con = uri.toURL.openConnection().asInstanceOf[HttpURLConnection]
          con.setRequestProperty("accept", "application/vnd.collection+json")
          val content = Source.fromInputStream(con.getInputStream, "utf-8").mkString("")
          val headers = con.getHeaderFields.toMap filter {case (key, _) => key != null}
          val result = NativeJsonCollectionParser.parseCollection(content)
          Ok ~> Html5(views.data(uri, params, result, CjResponse(con.getResponseCode, con.getResponseMessage, headers)))
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

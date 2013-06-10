package io.trygvis.cj

import scala.xml.{NodeSeq, Elem}
import java.net._
import org.json4s.native.JsonMethods
import java.io.{PrintWriter, StringWriter, Writer}
import scala.xml.Group
import scala.Some

class Views(baseUrl: String) {

  val notSet = <i>Not set</i>

  def tryLink(s: String) = try {
    val url = new URL(s)
    <a href={url.toExternalForm}>{s}</a>
  } catch {
    case _: MalformedURLException => <xml:group>{s}</xml:group>
  }

  def render(uri: URI) = {
    val s = uri.toURL.toExternalForm
    baseUrl + "render?url=" + URLEncoder.encode(s, "utf-8")
  }

  def delete(uri: URI) = {
    val s = uri.toURL.toExternalForm
    baseUrl + "render?url=" + URLEncoder.encode(s, "utf-8") + "&action=delete"
  }

  def examples: Array[Elem] = Array("minimal", "collection", "item", "queries", "template", "error") map { name =>
    val cj = baseUrl + "examples/from-spec/" + name + ".collection+json"
    <a href={render(URI.create(cj))}>{name}</a>
  }

  def getStackTrace(aThrowable: Throwable) = {
    val result: Writer = new StringWriter()
    val printWriter: PrintWriter = new PrintWriter(result)
    aThrowable.printStackTrace(printWriter)
    result.toString
  }

  def getName(link: Link, prefix: String, i: Int) = {
    prefix + link.name.orElse(link.prompt).getOrElse("#" + i)
  }

  def index = {
    def innerContent =
      <div class='hero-unit'>
        <h1>&#x2764; Collection+JSON &#x2764;</h1>

        <!-- spacers -->
        <div class='row-fluid'> &nbsp;</div>
        <div class='row-fluid'> &nbsp;</div>

        <div class='row-fluid'>
          <div class='span12 input-append'>
            <form action='/render' class='form-search'>
              <input type='text' name='url' placeholder='Resource to explore' class='span11'/>
              <button type='submit' class='btn btn-primary'>Explore</button>
            </form>
          </div>
        </div>
      </div>
      <div class="row-fluid">
        <div class="span4">
          <h2>About</h2>
          <p>This is an interactive explorer for the Collection+JSON hypermedia. Give it an URL and it will render is as good as it can.</p>
          <p>
            The purpose is twofold: it is a useful tool to inspect
            collections while developing or debugging an application. At
            the same time it's useful to show the power of
            <a href='http://en.wikipedia.org/wiki/Hypermedia'>hypermedia</a>
            by showing how much a generic user agent can do by using
            only the generic Collection+JSON specification and not
            knowing anything about your application.
          </p>
          <h3>See also</h3>
          <p>
            There's a growing C+J community that's discussing on the <a href="https://groups.google.com/forum/#!forum/collectionjson">Google Group</a>.
          </p>
          <p>
            Reading the (quite light) <a href="http://amundsen.com/media-types/collection/">formal specification</a> is
            useful. It also has a tutorial and some examples.
          </p>
        </div>
        <div class="span4">
          <h2>Using</h2>
          <p>
            Feel to use this service! However, note that it's running on a free
            <a href="http://heroku.com">Heroku</a>
            instance so it might fail, be slow or otherwise useless.</p>
          <p>
            If you want to run it against your own local servers you can either run it yourself, or use apps like
            <a href="http://localtunnel.com">localtunnel.com</a> to make your application publicly available.
          </p>
          <h3>The Source Code</h3>
          <p>
            The source code is available
            <a href="http://trygvis.dyndns.org/~trygvis/git/2012/06/collection+json-explorer.git">here</a>.
          </p>
        </div>
        <div class="span4">
          <h2>Examples</h2>
          <p>
            The <a href="http://employee.herokuapp.com">Employee</a> application is a set of resources with employees
            and departments. The application was made specifically for this explorer:
            <a href={render(URI.create("http://employee.herokuapp.com"))} class="label label-info">explore now!</a>
          </p>
          <p>The specification contains a few example collections too which
            you can explore:
          </p>
          <ul>
            {examples.map {example => <li>{example}</li>}}
          </ul>
        </div>
      </div>
    ;

    def content = <div class="offset2 span10">{innerContent}</div>
    layout(content, None)
  }

  def data(request: CjRequest, response: Option[Either[Throwable, CjResponse]],
           targetParams: Map[String, String], collection: Option[Either[Throwable, Collection]]) = {
    def href(uri: URI) = {
//      val splits = uri.getPath.split('/')
//      for split in splits
//      a(href=url generator.render(split[1]), title='Explore #{split[1]}') #{split[0]}
      <a href={uri.toURL.toExternalForm}>{uri.toURL.toExternalForm}</a>
    }

    def link(link: Link) = {
      <xml:group>
      <div>
        <a class="btn btn-primary btn-mini" href={render(link.href)}>Explore</a>
        <a class="btn btn-primary btn-mini" href={link.href.toURL.toExternalForm}>Raw</a>
      </div>
      <dl>
        <dt>href</dt>
        <dd><div>{href(link.href)}</div></dd>
        <dt>rel</dt>
        <dd>{tryLink(link.rel)}</dd>
        <dt>name</dt>
        <dd>{link.name.getOrElse(notSet)}</dd>
        <dt>prompt</dt>
        <dd>{link.prompt.getOrElse(notSet)}</dd>
        <dt>render</dt>
        <dd>{link.render.getOrElse(notSet)}</dd>
        {if(link.render == "image")
          <dt>Image</dt>
          <dd>
            <a href={link.href.toURL.toExternalForm}>
              <img src={link.href.toURL.toExternalForm} alt={link.name.getOrElse("")} title={link.name.getOrElse("")}/>
            </a>
          </dd>
        }
      </dl>
      </xml:group>
    }

    def meta(implicit cj: Collection) = <xml:group>
      <div class="row-fluid">
        <div class="span12">
          <dl>
            <dt>version</dt>
            <dd>{cj.version}</dd>
            <dt>href</dt>
            <dd>{cj.href.map(href).getOrElse(notSet)}</dd>
          </dl>
        </div>
      </div>
      {if(cj.href.isDefined) {
        val url = cj.href.get
        <div class="row-fluid">
          <div class="span12">
            <p>
              <a class="btn btn-primary" href={render(url)}>Explore</a>
              <a class="btn btn-primary" href={url.toURL.toExternalForm}>Raw</a>
              <a class="btn btn-danger" href={delete(url)}>Delete</a>
              <form action='http://redbot.org'>
                <input name='uri' value={url.toURL.toExternalForm} type='hidden'/>
                <input name='req_hdr' value='Accept: application/vnd.collection+json' type='hidden'/>
                <button class='btn btn-primary' type='submit'>Check with redbot.org</button>
              </form>
            </p>
          </div>
        </div>
      }}
      {cj.links match {
        case Nil =>
          NodeSeq.Empty
        case _ =>
          <xml:group>
            <h2>Collection Links</h2>
            {cj.links.zipWithIndex.map { case (l, i) =>
              val title = l.prompt.orElse(l.name).map(": " + _).getOrElse("")
              Group(Seq(<h3 id={"link-" + (i + 1)}>{"Collection link #" + (i + 1) + title}</h3>, link(l)))
            }}
          </xml:group>
      }}
      </xml:group>

    // TODO: If the collection has prev/next links, add buttons to automatically navigate those.
    // TODO: Add ability to show the raw part of the collection.
    def items(cj: Collection) = {

      def itemLinks(cj: Collection) = {
        val first = cj.findLinkByRel("first")
        val prev = cj.findLinkByRel("prev")
        val next = cj.findLinkByRel("next")
        val last = cj.findLinkByRel("last")
        if(first.isDefined || prev.isDefined || next.isDefined || last.isDefined) {
          <div class="fluid-row">
            <div class="span12">
              {if(first.isDefined) <a class="btn btn-primary btn-mini" href={render(first.get.href) + "#items"}>First</a>}
              {if(prev.isDefined) <a class="btn btn-primary btn-mini" href={render(prev.get.href) + "#items"}>Previous</a>}
              {if(next.isDefined) <a class="btn btn-primary btn-mini" href={render(next.get.href) + "#items"}>Next</a>}
              {if(last.isDefined) <a class="btn btn-primary btn-mini" href={render(last.get.href) + "#items"}>Last</a>}
            </div>
          </div>
        }
      }

      <xml:group>
        {itemLinks(cj)}
        {
          cj.items.zipWithIndex.map { case (item, i) =>
            val links = item.links
            <div class="item-container">
              <h2 id={"item-" + (i + 1)}>Item #{i + 1}</h2>
              {if(item.href.isDefined) {
              val url = item.href.get
              <div class="fluid-row">
                <div class="span12">
                  <p>
                    <a class="btn btn-primary btn-mini" href={render(url)}>Explore</a>
                    <a class="btn btn-primary btn-mini" href={url.toURL.toExternalForm}>Raw</a>
                    <a class="btn btn-primary btn-mini" onClick='var item = $(this).parentsUntil("#items").last(); item.find(".item-form").toggle(); item.find(".item-data").toggle()'>Edit</a>
                    <a class="btn btn-danger btn-mini" href={delete(url)}>Delete</a>
                  </p>
                </div>
              </div>
              }}
              <div class="fluid-row">
                <div class="span12">
                  <dl>
                    <dt>href</dt>
                    <dd><div>{item.href.map(href).getOrElse(notSet)}</div></dd>
                  </dl>
                </div>
              </div>
              {if(links.nonEmpty) {
                <xml:group>
                <h3>Item Links</h3>
                {links.zipWithIndex.map { case (l, i2) =>
                  Group(Seq(<h4>Item Link #{i2 + 1}</h4>, link(l)))
                }}
                </xml:group>
              }}
              <h3 class="item-data">Data</h3>
              <div class="item-data fluid-row">
                <div class="span12">
                  <table class="data-table">
                    {item.data map { d => <tr><th>{d.name}</th><td>{d.value getOrElse ""}</td></tr>}}
                  </table>
                </div>
              </div>
              {if(item.href.isDefined) {
                val uri = item.href.get
              <xml:group>
              <h3 class="item-data" style="display: none">Data</h3>
              <div class="item-data fluid-row" style="display: none">
                <div class="span12">
                  <form class="well" action="/write" method="POST">
                    <input type="hidden" name="url" value={uri.toURL.toExternalForm}/>
                    <table class="cj-form">
                      <tbody>{
                          item.data map { d =>
                            val value = d.value getOrElse ""
                            <tr>
                              <th title={"name: " + d.name}>
                                <label for={d.name}>{d.prompt.getOrElse(d.name)}</label>
                              </th>
                              <td>
                                <input id={d.name} type="text" name={"param-" + d.name} value={value}/>
                              </td>
                            </tr>
                          }
                      }
                      </tbody>
                      <tfoot>
                        <tr>
                          <th></th>
                          <td>
                            <p>
                              <input class="btn btn-primary" type="submit">Update</input>
                            </p>
                          </td>
                        </tr>
                      </tfoot>
                    </table>
                  </form>
                </div>
              </div>
              </xml:group>
              }}
            </div>
          }
        }
        {itemLinks(cj)}
      </xml:group>
    }

    def queries(implicit cj: Collection) = {
      {cj.queries.zipWithIndex map { case (query, i) =>
        val prompt = query.prompt
        val name = query.name
        val title = prompt.orElse(name).getOrElse("Unnamed query #" + (i + 1))

        <h2 id={"query-" + (i + 1)}>{title}</h2>
        <div class="row-fluid">
          <div class="span12">
            <form class="well" action="/render">
              <input type="hidden" name="url" value={query.href.toURL.toExternalForm}/>
              <table class="cj-form">
                <tbody>{query.data map { d =>
                  val value = targetParams.get(d.name).orElse(d.value) getOrElse ""
                  <tr>
                    <th title={"name: " + d.name}>
                      <div>
                        <label for={d.name}>{d.prompt.getOrElse(d.name)}</label>
                      </div>
                    </th>
                    <td>
                      <input id={d.name} type="text" name={"param-" + d.name} value={value}/>
                    </td>
                  </tr>
                }}</tbody>
                <tfoot>
                  <tr>
                    <th></th>
                    <td>
                      <input class="btn btn-primary" type="submit" value="Query"/>
                    </td>
                  </tr>
                </tfoot>
              </table>
            </form>
          </div>
        </div>
      }}
    }

    def template(implicit cj: Collection) =
      <div class="row-fluid">
        <div class="span12">
          <form class="well" action="/write" method="POST">
            {cj.href map {uri =>
            <xml:group>
              <p>The data will be submitted to: {href(uri)}</p>
              <input type="hidden" name="url" value={uri.toURL.toExternalForm}/>
            </xml:group>
            } getOrElse NodeSeq.Empty}
            <table class="cj-form">
              <tbody>{cj.template.get.data map { d =>
                val value = targetParams.get(d.name).orElse(d.value).getOrElse("")
                <tr>
                  <th title={"name='" + d.name + "'"}>
                    <div>
                      <label for={d.name}>{d.prompt.getOrElse(d.name)}</label>
                    </div>
                  </th>
                  <td>
                    <input id={d.name} type="text" name={"param-" + d.name} value={value}/>
                  </td>
                </tr>
              }}</tbody>
              <tfoot>
                <tr>
                  <th></th>
                  <td>
                    {cj.href match {
                    case Some(uri) =>
                      <input class="btn btn-primary" type="submit" value="Write"/>
                    case None =>
                      <input class="btn btn-primary disabled" type="submit" disabled="disabled" value="Write"/>
                      <p class="help-block">This collection has a template, but doesn't have a href which is required.</p>
                    }}
                  </td>
                </tr>
              </tfoot>
            </table>
          </form>
        </div>
      </div>

    def error(implicit cj: Collection) = {
      val e = cj.error.get
      val message = e.message map { m =>
        val lines = m.split('\n')
        lines.map(s => scala.xml.Text(s): NodeSeq).reduce(_ ++ <br /> ++ _)
      }

      <div class="row-fluid">
        <dl>
          <dt>title</dt>
          <dd>{Option(e.title).filter(!_.isEmpty).getOrElse(notSet)}</dd>
          <dt>code</dt>
          <dd>{e.code.getOrElse(notSet)}</dd>
          <dt>message</dt>
          <dd>{message.getOrElse("")}</dd>
        </dl>
      </div>
    }

    def httpRequest(req: CjRequest) = {
      <div class="row-fluid">
        {req.url match {
          case None =>
            <dl>
              <dt>Could not parse generated URL</dt>
              <dd>{req.urlString}</dd>
            </dl>
          case _ =>
        }}
        {
        req.headers match {
          case None =>
          case Some(headers) =>
            <h2>Request</h2>
            <table>
              <tr>
                <td colspan="2"><tt>{req.method} {req.url map { url => <a href={render(url.toURI)}>{url.toExternalForm}</a> } getOrElse req.urlString}</tt></td>
              </tr>
              {headers map { case (header, values) => { values map { value =>
              <tr>
                <td><tt>{header}: </tt></td>
                <td><tt>{tryLink(value)}</tt></td>
              </tr>
            }}}}
            </table>
        }}
      </div>
    }

    def httpResponse(r: Either[Throwable, CjResponse]) = { r match {
      case Left(ex) =>
        ex match {
          case x: ConnectException => "Unable to connect: " + x.getMessage
          case x: UnknownHostException =>  "Unknown host: " + x.getMessage
          case _ => <xml:group>
            <p>Unknown error while connecting to remote server</p>
            <pre>{getStackTrace(ex)}</pre>
          </xml:group>
        }
      case Right(res) =>
        <div class="row-fluid">
          <tt>
          <table>
            <tr>
              <td colspan="2">{res.code} {res.status}</td>
            </tr>
            {res.headers map { case (header, values) => { values map { value =>
              <tr>
                <td>{header}: </td>
                <td>{tryLink(value)}</td>
              </tr>
            }}}}
          </table>
          </tt>
        </div>
    }}

    def parsedContent(implicit cj: Collection) = <xml:group>
      <section id="meta">
        <div class="page-header">
          <h1>Meta</h1>
        </div>
        {meta}
      </section>
      {if(cj.items.nonEmpty) {
        <section id="items">
          <div class="page-header">
            <h1>
              Items
              {if(cj.items.length > 1) {
              <span class="badge">
                {cj.items.length}
                {if(cj.findLinkByRel("next").isDefined) { "+" } }
              </span>
            }}
            </h1>
          </div>
          {items(cj)}
        </section>
      }}
      {if(cj.queries.nonEmpty) {
        <section id="queries">
          <div class="page-header"><h1>Queries</h1></div>
          {queries}
        </section>
      }}
      {if(cj.template.isDefined) {
        <section id="template">
          <div class="page-header"><h1>Template</h1></div>
          {template}
        </section>
      }}
      {if(cj.error.isDefined) {
        <section id="error">
          <div class="page-header"><h1>Error</h1></div>
          {error}
        </section>
      }}
    </xml:group>

    def innerContent = <xml:group>
      {collection match {
        case None =>
        case Some(Left(ex)) =>
          <section id="server-error">
            <div class="page-header"><h1>Server Error</h1></div>
            <pre>{getStackTrace(ex)}</pre>
          </section>
        case Some(Right(cj)) => <xml:group>
          {try { parsedContent(cj) } catch { case ex: Exception => <div>Unable to process model: {ex.getMessage}</div> }}
          <section id="formatted-body">
            <div class="page-header"><h1>Formatted Body</h1></div>
            <div class="row-fluid">
              <div class="span12">
                <pre>{JsonMethods.pretty(JsonMethods.render(cj.underlying))}</pre>
              </div>
            </div>
          </section>
        </xml:group>
      }}
      <section id="http-request">
        <div class="page-header"><h1>HTTP Request</h1></div>
        {httpRequest(request)}
      </section>
      {response match {
        case None =>
        case Some(some) =>
          <section id="http-response">
            <div class="page-header"><h1>HTTP Response</h1></div>
            {httpResponse(some)}
          </section>
      }}
    </xml:group>

    def sidebar = <div id="navbar" class="sidebar-nav sidebar-nav-fixed">
      <ul class="nav nav-list">
        {collection match {
        case None =>
        case Some(Left(_)) =>
          <li class="nav-header"><a href="#server-error">Server Error</a></li>
        case Some(Right(cj)) =>
          {try {
            <xml:group>
              <li class="nav-header active"><a href="#meta">Meta</a></li>
              {cj.links.zipWithIndex map { case (l, i) =>
                <li><a href={"#link-" + (i + 1)}>{getName(l, "Link ", i + 1)}</a></li>
              }}
              {if(cj.items.nonEmpty) {
                <xml:group>
                <li class="nav-header"><a href="#items">Items</a></li>
                {cj.items.zipWithIndex.map { case (_, i) =>
                  <li><a href={"#item-" + (i + 1)}>#{i + 1}</a></li>
                }}
                </xml:group>
              }}
              {if(cj.queries.nonEmpty) {
                <xml:group>
                <li class="nav-header"><a href="#queries">Queries</a></li>
                {cj.queries.zipWithIndex.map { case (_, i) =>
                  <li><a href={"#query-" + (i + 1)}>#{i + 1}</a></li>
                }}
                </xml:group>
              }}
              {cj.template map { _ => <li class="nav-header"><a href="#template">Template</a></li> } getOrElse NodeSeq.Empty }
              {cj.error map { _ => <li class="nav-header"><a href="#error">Error</a></li> } getOrElse NodeSeq.Empty }
              <li class="nav-header"><a href="#formatted-body">Formatted Body</a></li>
            </xml:group>
          } catch { case ex: Exception => <div>Unable to process model: {ex.getMessage}</div> }}
        }}
        <li class="nav-header"><a href="#http-request">HTTP Request</a></li>
        {
        response match {
          case None =>
          case Some(some) =>
            <li class="nav-header"><a href="#http-response">HTTP Response</a></li>
            /*
            <xml:group>
              <li class="nav-header"><a href="#http-response">HTTP Response</a></li>
              {some match {
                case Right(_) =>
                  <li class="nav-header"><a href="#formatted-body">Formatted Body</a></li>
                case _ =>
              }}
            </xml:group>
            */
        }}
      </ul>
    </div>

    def content =
      <div class="row-fluid">
        <div class="span3">{sidebar}</div>
        <div class="span9">{innerContent}</div>
      </div>

    layout(content, None)
  }

  def layout(content: Elem, headSnippet: Option[String]) =
    <html>
      <head>
        <title>Collection+JSON Explorer</title>
        <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
        <link href="/bootstrap-2.0.4/css/bootstrap.css" rel="stylesheet"/>
        <link href="/bootstrap-2.0.4/css/bootstrap-responsive.css" rel="stylesheet"/>
        <link href="/stylesheets/style.css" rel="stylesheet"/>
        <link href="/bootstrap-2.0.4/css/bootstrap-responsive.css" rel="stylesheet"/>
        <!--[if lt IE 9]><script src="http://html5shim.googlecode.com/svn/trunk/html5.js"></script><![endif]-->
        <script>
          {headSnippet.getOrElse("")}
        </script>
      </head>

      <body data-spy="scroll">
        <div class="container-fluid">
          {content}
        </div>

        <script src="/javascripts/jquery-1.7.2.min.js"></script>
        <script src="/bootstrap-2.0.4/js/bootstrap.js"></script>
      </body>
    </html>
}

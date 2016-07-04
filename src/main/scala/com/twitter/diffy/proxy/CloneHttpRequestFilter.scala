package com.twitter.diffy.proxy

import com.twitter.finagle.Filter
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpHeaders, DefaultHttpRequest, HttpResponse, HttpRequest}
import scala.collection.JavaConverters._

object CloneHttpRequestFilter {

  type Req = HttpRequest
  type Res = HttpResponse

  private def mkCloneRequest(reqIn:Req, filter: (Req => Future[Res])):Future[Res] = {
    def copyHeader(headers:HttpHeaders)(k: String, v:String) : Unit = { headers.add(k, v)}

    val reqOut: DefaultHttpRequest = new DefaultHttpRequest(reqIn.getProtocolVersion, reqIn.getMethod, reqIn.getUri)
    reqOut.setChunked(reqIn.isChunked)
    reqOut.setContent(reqIn.getContent)

    val headers = for {entry <- reqIn.headers().asScala} yield (entry.getKey, entry.getValue)
    headers.foreach((copyHeader(reqOut.headers)_).tupled)

    filter(reqOut)
  }

  def apply(): Filter[Req, Res, Req, Res] = Filter.mk(mkCloneRequest)
}

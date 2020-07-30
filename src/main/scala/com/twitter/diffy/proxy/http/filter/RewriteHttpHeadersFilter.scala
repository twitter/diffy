package com.twitter.diffy.proxy.http.filter

import com.twitter.finagle.Filter
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpHeaders, HttpResponse, HttpRequest}
import scala.collection.JavaConverters._

object RewriteHttpHeadersFilter {

  def apply(rules:Seq[HeaderRule]) : Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse] = {
    def filter(req: HttpRequest):HttpRequest = {
      val headers: HttpHeaders = req.headers
      val effects = for {
        name <- headers.names().asScala
        effect <- rules.flatMap(_(name)).headOption
      } yield {
        effect
      }
      effects.foreach(_ (headers))
      req
    }

    def go(req: HttpRequest, next: (HttpRequest => Future[HttpResponse])): Future[HttpResponse] ={
      next(filter(req))
    }

    Filter.mk(go)
  }
}

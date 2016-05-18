package com.twitter.diffy.proxy

import com.twitter.finagle.Service
import com.twitter.util.{Future, Try}

import org.jboss.netty.handler.codec.http.HttpRequest

case class Server(classifier: Int)

class SequentialMulticastService[-A, +B, C](
    services: Seq[Service[A, B]], headerPairs: Seq[C])
  extends Service[A, Seq[Try[B]]]
{
  var requestCount = 0
  var headersApplied = ""

  def applyHeaders(server: Server, request: A): Unit = {
    val httpHeaders = server match {
      case Server(0) => headerPairs(0).toString
      case Server(1) => headerPairs(1).toString
      case Server(2) => headerPairs(2).toString
    }
    if (request.isInstanceOf[HttpRequest]) {
      for ( headers <-httpHeaders.split(",") ) {
          val valuePair = headers.split(":").map(_.trim)
          if (valuePair.length == 2) {
            request.asInstanceOf[HttpRequest].headers().add(valuePair(0), valuePair(1))
            headersApplied += valuePair(0) + ","
          }
      }
    }
  }

  def unapplyHeaders(request: A): Unit = {
    if (request.isInstanceOf[HttpRequest]) {
      for ( headers <- headersApplied.split(",") )
          request.asInstanceOf[HttpRequest].headers().remove(headers)
    }
  }

  def apply(request: A): Future[Seq[Try[B]]] =
    services.foldLeft[Future[Seq[Try[B]]]](Future.Nil){ case (acc, service) =>
      acc flatMap { responseTries =>
        if (requestCount > 0)
          unapplyHeaders(request)
        applyHeaders(Server(requestCount),request)
        requestCount += 1
        if (requestCount == 3)
          requestCount = 0
        val nextResponse = service(request).liftToTry
        nextResponse map { responseTry => responseTries ++ Seq(responseTry) }

      }
    }
}

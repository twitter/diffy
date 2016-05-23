package com.twitter.diffy.proxy

import com.twitter.finagle.Service
import com.twitter.util.{Future, Try}

import org.jboss.netty.handler.codec.http.HttpRequest

case class Server(classifier: Int)

class SequentialMulticastService[-A, +B, C, D](
                                                services: Seq[Service[A, B]], headerPairs: Seq[C], apiRoots: Seq[D])
  extends Service[A, Seq[Try[B]]]
{
  var requestCount = 0
  var headersApplied = ""
  var dest = ""

  def applyHeaders(server: Server, request: HttpRequest): Unit = {
    val httpHeaders = server match {
      case Server(0) => headerPairs(0).toString
      case Server(1) => headerPairs(1).toString
      case Server(2) => headerPairs(2).toString
    }

      for ( headers <-httpHeaders.split(",") ) {
        val valuePair = headers.split(":").map(_.trim)
        if (valuePair.length == 2) {
          request.headers.add(valuePair(0), valuePair(1))
          headersApplied += valuePair(0) + ","
        }
      }

  }

  def addApiRoot(server: Server, request: HttpRequest): Unit = {
    val apiRoot = server match {
      case Server(0) => apiRoots(0).toString
      case Server(1) => apiRoots(1).toString
      case Server(2) => apiRoots(2).toString
    }

    request.setUri(apiRoot + dest)
  }

  def unapplyHeaders(request: HttpRequest): Unit = {
    for ( headers <- headersApplied.split(",") )
        request.headers.remove(headers)
  }

  def substituteParamsWith(server: Server, toBeSubstituted: String, request: HttpRequest): Unit = {
    val apiParamElem = scala.xml.XML.loadFile(System.getProperty("user.dir") + "/apiParams.xml")
    val substitutedString = server match {
      case Server(0) => (apiParamElem \ toBeSubstituted \ "primary").text.trim
      case Server(1) => (apiParamElem \ toBeSubstituted \ "candidate").text.trim
      case Server(2) => (apiParamElem \ toBeSubstituted \ "secondary").text.trim
    }
    request.setUri(request.getUri.replace(toBeSubstituted,substitutedString))
  }

  def substituteParams(server: Server, request: HttpRequest): Unit = {

    if (request.getUri.split('?').length == 2) {
      val params = request.getUri.split('?')(1).split('&')
      if (params.length > 1) {
        for ( i <- 0 until params.length ){
          if (params(i).split('=').length == 2)
            substituteParamsWith(server, params(i).split('=')(1), request)
        }
      }
      else {
        if (params(0).split('=').length == 2)
          substituteParamsWith(server, params(0).split('=')(1), request)
      }
    }

  }

  def apply(request: A): Future[Seq[Try[B]]] =
    services.foldLeft[Future[Seq[Try[B]]]](Future.Nil){ case (acc, service) =>
      acc flatMap { responseTries =>
        if (dest.equals(""))
          dest = request.asInstanceOf[HttpRequest].getUri
        if (requestCount > 0)
          unapplyHeaders(request.asInstanceOf[HttpRequest])
        applyHeaders(Server(requestCount),request.asInstanceOf[HttpRequest])
        addApiRoot(Server(requestCount),request.asInstanceOf[HttpRequest])
        substituteParams(Server(requestCount),request.asInstanceOf[HttpRequest])
        requestCount += 1
        if (requestCount == 3)
          requestCount = 0
        val nextResponse = service(request).liftToTry
        nextResponse map { responseTry => responseTries ++ Seq(responseTry) }

      }
    }
}
package com.twitter.diffy.proxy

import com.twitter.finagle.Service
import com.twitter.util.{Future, Try}

import org.jboss.netty.handler.codec.http.HttpRequest

case class Server(classifier: Int)

class SequentialMulticastService[-A, +B, C, D](
                                                services: Seq[Service[A, B]], headerPairs: Seq[C], apiRoots: Seq[D])
  extends Service[A, Seq[Try[B]]]
{
  var requestCount = 0
  var headersApplied = ""
  var dest = ""

  def applyHeaders(server: Server, request: HttpRequest): Unit = {
    val httpHeaders = server match {
      case Server(0) => headerPairs(0).toString
      case Server(1) => headerPairs(1).toString
      case Server(2) => headerPairs(2).toString
    }

      for ( headers <-httpHeaders.split(",") ) {
        val valuePair = headers.split(":").map(_.trim)
        if (valuePair.length == 2) {
          request.headers.add(valuePair(0), valuePair(1))
          headersApplied += valuePair(0) + ","
        }
      }

  }

  def addApiRoot(server: Server, request: HttpRequest): Unit = {
    val apiRoot = server match {
      case Server(0) => apiRoots(0).toString
      case Server(1) => apiRoots(1).toString
      case Server(2) => apiRoots(2).toString
    }

    request.setUri(apiRoot + dest)
  }

  def unapplyHeaders(request: HttpRequest): Unit = {
    for ( headers <- headersApplied.split(",") )
        request.headers.remove(headers)
  }

  def substituteParamsWith(server: Server, toBeSubstituted: String, request: HttpRequest): Unit = {
    val apiParamElem = scala.xml.XML.loadFile(System.getProperty("user.dir") + "/apiParams.xml")
    val substitutedString = server match {
      case Server(0) => (apiParamElem \ toBeSubstituted \ "primary").text.trim
      case Server(1) => (apiParamElem \ toBeSubstituted \ "candidate").text.trim
      case Server(2) => (apiParamElem \ toBeSubstituted \ "secondary").text.trim
    }
    request.setUri(request.getUri.replace(toBeSubstituted,substitutedString))
  }

  def substituteParams(server: Server, request: HttpRequest): Unit = {

    if (request.getUri.split('?').length == 2) {
      val params = request.getUri.split('?')(1).split('&')
      if (params.length > 1) {
        for ( i <- 0 until params.length ){
          if (params(i).split('=').length == 2)
            substituteParamsWith(server, params(i).split('=')(1), request)
        }
      }
      else {
        if (params(0).split('=').length == 2)
          substituteParamsWith(server, params(0).split('=')(1), request)
      }
    }

  }

  def apply(request: A): Future[Seq[Try[B]]] =
    services.foldLeft[Future[Seq[Try[B]]]](Future.Nil){ case (acc, service) =>
      acc flatMap { responseTries =>
        if (dest.equals(""))
          dest = request.asInstanceOf[HttpRequest].getUri
        if (requestCount > 0)
          unapplyHeaders(request.asInstanceOf[HttpRequest])
        applyHeaders(Server(requestCount),request.asInstanceOf[HttpRequest])
        addApiRoot(Server(requestCount),request.asInstanceOf[HttpRequest])
        substituteParams(Server(requestCount),request.asInstanceOf[HttpRequest])
        requestCount += 1
        if (requestCount == 3)
          requestCount = 0
        val nextResponse = service(request).liftToTry
        nextResponse map { responseTry => responseTries ++ Seq(responseTry) }

      }
    }
}
package com.twitter.diffy.proxy

import com.twitter.finagle.Service
import com.twitter.util.{Future, Try}

import org.jboss.netty.handler.codec.http.HttpRequest

case class Server(classifier: Int)

class SequentialMulticastService[-A, +B, C, D](
                                                services: Seq[Service[A, B]], headerPairs: Seq[C], apiRoots: Seq[D])
  extends Service[A, Seq[Try[B]]]
{
  var requestCount = 0
  var headersApplied = ""
  var dest = ""

  def applyHeaders(server: Server, request: HttpRequest): Unit = {
    val httpHeaders = server match {
      case Server(0) => headerPairs(0).toString
      case Server(1) => headerPairs(1).toString
      case Server(2) => headerPairs(2).toString
    }

      for ( headers <-httpHeaders.split(",") ) {
        val valuePair = headers.split(":").map(_.trim)
        if (valuePair.length == 2) {
          request.headers.add(valuePair(0), valuePair(1))
          headersApplied += valuePair(0) + ","
        }
      }

  }

  def addApiRoot(server: Server, request: HttpRequest): Unit = {
    val apiRoot = server match {
      case Server(0) => apiRoots(0).toString
      case Server(1) => apiRoots(1).toString
      case Server(2) => apiRoots(2).toString
    }

    request.setUri(apiRoot + dest)
  }

  def unapplyHeaders(request: HttpRequest): Unit = {
    for ( headers <- headersApplied.split(",") )
        request.headers.remove(headers)
  }

  def substituteParamsWith(server: Server, toBeSubstituted: String, request: HttpRequest): Unit = {
    val apiParamElem = scala.xml.XML.loadFile(System.getProperty("user.dir") + "/apiParams.xml")
    val substitutedString = server match {
      case Server(0) => (apiParamElem \ toBeSubstituted \ "primary").text.trim
      case Server(1) => (apiParamElem \ toBeSubstituted \ "candidate").text.trim
      case Server(2) => (apiParamElem \ toBeSubstituted \ "secondary").text.trim
    }
    request.setUri(request.getUri.replace(toBeSubstituted,substitutedString))
  }

  def substituteParams(server: Server, request: HttpRequest): Unit = {

    if (request.getUri.split('?').length == 2) {
      val params = request.getUri.split('?')(1).split('&')
      if (params.length > 1) {
        for ( i <- 0 until params.length ){
          if (params(i).split('=').length == 2)
            substituteParamsWith(server, params(i).split('=')(1), request)
        }
      }
      else {
        if (params(0).split('=').length == 2)
          substituteParamsWith(server, params(0).split('=')(1), request)
      }
    }

  }

  def apply(request: A): Future[Seq[Try[B]]] =
    services.foldLeft[Future[Seq[Try[B]]]](Future.Nil){ case (acc, service) =>
      acc flatMap { responseTries =>
        if (dest.equals(""))
          dest = request.asInstanceOf[HttpRequest].getUri
        if (requestCount > 0)
          unapplyHeaders(request.asInstanceOf[HttpRequest])
        applyHeaders(Server(requestCount),request.asInstanceOf[HttpRequest])
        addApiRoot(Server(requestCount),request.asInstanceOf[HttpRequest])
        substituteParams(Server(requestCount),request.asInstanceOf[HttpRequest])
        requestCount += 1
        if (requestCount == 3)
          requestCount = 0
        val nextResponse = service(request).liftToTry
        nextResponse map { responseTry => responseTries ++ Seq(responseTry) }

      }
    }
}

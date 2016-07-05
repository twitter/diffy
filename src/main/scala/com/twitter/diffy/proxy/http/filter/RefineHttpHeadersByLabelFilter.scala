package com.twitter.diffy.proxy.http.filter

import com.twitter.finagle.Filter
import com.twitter.finagle.http.Request
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpHeaders, HttpRequest, HttpResponse}

import scala.collection.JavaConverters._

object RefineHttpHeadersByLabelFilter {
  val knownLabels: Seq[String] = Seq("primary", "secondary", "candidate")

  def substitute(inclusions: Seq[String], exclusion: Seq[String])(req: HttpRequest) : HttpRequest = {

    def orFn[T](f: T => Boolean, g: T => Boolean): T => Boolean = { t =>  f(t) || g(t) }
    def constFalse[T](t: T): Boolean = false

    def renameHeaders(headers: HttpHeaders)(nameOld: String, nameNew: String): Unit = {
      val temp = headers.get(nameOld)
      headers.remove(nameOld)
      headers.set(nameNew, temp)
    }


    val removeExclusion  = exclusion.
      map(_+"_").
      map( x => (_:String).startsWith(x)).
      foldLeft(constFalse[String]_)(orFn)

    val reqOut = Request.apply(req)
    val headers: HttpHeaders = reqOut.headers
    //k => Option[Function[k => k]]
    val incomingHeaders = headers.names().asScala.toSet

    val rewrites = for {
      name <- incomingHeaders
      inclusion <- inclusions if name.startsWith(inclusion + "_")
    } yield name->name.substring(inclusion.length+1)

    val (removals, rewrites2) = rewrites.partition(_._2.isEmpty)
    incomingHeaders.filter( removeExclusion).foreach( headers.remove )
    removals.map(_._1).foreach( headers.remove )
    rewrites2.foreach((renameHeaders(headers) _ ).tupled)

    reqOut
  }

  def apply(label:String): Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse] = {
    def mkFilter(inclusions: Seq[String], exclusion: Seq[String])
                (req:HttpRequest, filter: (HttpRequest => Future[HttpResponse]) ) =
      filter(substitute(inclusions, exclusion)(req))

    val (inclusions, exclusion) = knownLabels.partition(_.equalsIgnoreCase(label))
    Filter.mk( mkFilter(inclusions, exclusion))
  }

}

package com.twitter.diffy.proxy.http.filter

import com.twitter.diffy.ParentSpec
import com.twitter.diffy.util.TwitterFutures
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http.{HttpHeaders, HttpRequest, HttpResponse}
import org.junit.runner.RunWith
import org.scalatest.FunSpec
import org.scalatest.junit.JUnitRunner

import scala.collection.JavaConverters._

trait HeaderTransformations extends ParentSpec with TwitterFutures {  this: FunSpec =>

  def returnHeaders(req: HttpRequest): Future[HttpResponse] = {
    val response: Response = Response(req)
    response.headers().add(req.headers())
    Future.apply(response)
  }

  def anEchoServiceForHeaders(subject: Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse],
                              inputHeaders: Map[String,String],
                              expectedHeaders: Map[String,String]): Unit ={
    val service = subject.andThen(Service.mk(returnHeaders))

    it ("must match headers"){
      val req1: HttpRequest = Request("/")
      val addHeader = (k:String, v:String) => req1.headers().add(k, v)
      inputHeaders.foreach( addHeader.tupled )

      def headersToMap(httpHeaders:HttpHeaders): Map[String,String] = {
        httpHeaders.iterator().asScala.map( e=>(e.getKey,e.getValue )).toMap
      }
      whenReady(service(req1).map( _.headers ).map(headersToMap))( _ mustBe expectedHeaders )
    }
  }

  def aFilterThatTargetsLabel(label:String, labelEx1:String, labelEx2:String) : Unit ={
    describe(s"given a $label filter"){
      val subject: Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse] =
        RefineHttpHeadersByLabelFilter(label, List(label, labelEx1, labelEx2))

      describe("given empty headers"){
        it must behave like anEchoServiceForHeaders(subject, Map.empty, Map.empty)
      }
      describe("given a set of headers"){
        val headers=Map(("A", "B"), ("C-c", "D-d"))
        it must behave like anEchoServiceForHeaders(subject, headers, headers)
      }
      describe(s"given an exact $label header"){
        it must behave like anEchoServiceForHeaders(subject, Map( (s"$label", "X")), Map((s"$label", "X")))
      }
      describe(s"given a $label prefixed header"){
        it must behave like anEchoServiceForHeaders(subject, Map( (s"${label}_X", "X")), Map(("X", "X")))
      }
      describe(s"given a $label prefixed header with no suffix"){
        it must behave like anEchoServiceForHeaders(subject, Map( (s"${label}_", "X")), Map.empty)
      }

      describe(s"given a $labelEx1 prefixed header"){
        it must behave like anEchoServiceForHeaders(subject, Map( (s"${labelEx1}_X", "X")), Map.empty)
      }
      describe(s"given a $labelEx2 prefixed header"){
        it must behave like anEchoServiceForHeaders(subject, Map( (s"${labelEx2}_Y", "Y")), Map.empty)
      }

    }
  }

}
@RunWith(classOf[JUnitRunner])
class RefineHttpHeadersByLabelSpec extends ParentSpec with TwitterFutures with HeaderTransformations {
  describe("RefineHttpHeadersByLabelFilter"){
    it must behave like aFilterThatTargetsLabel("primary", "candidate", "secondary")
    it must behave like aFilterThatTargetsLabel("candidate", "primary", "secondary")
    it must behave like aFilterThatTargetsLabel("secondary", "primary", "candidate")
  }
}

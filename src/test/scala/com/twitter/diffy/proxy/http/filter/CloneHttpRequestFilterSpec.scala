package com.twitter.diffy.proxy.http.filter;

import com.twitter.diffy.ParentSpec
import com.twitter.diffy.util.TwitterFutures
import com.twitter.finagle.http.{Request, Response}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import org.jboss.netty.handler.codec.http._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CloneHttpRequestFilterSpec extends ParentSpec with TwitterFutures{
  describe("CloneHttpRequestFilter"){
    val subject: Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse] = CloneHttpRequestFilter.apply()

    def mutateHeader(req: HttpRequest): Future[HttpResponse] = {
      req.headers().add("mutation", "test")
      Future.apply(Response(req))
    }

    val mutateHeaderService =subject.andThen(Service.mk(mutateHeader))


    describe("recieving a request with no headers"){
      def request:Request={ Request(HttpVersion.HTTP_1_1, HttpMethod.GET, "/") }

      it("must prevent services which mutate the output request header from affecting the input request"){
        val input: Request = request
        whenReady(mutateHeaderService(input)){ _ => input.headers().names() mustNot contain("mutation") }
      }
      it("must preserve http method"){
        val input = request
        whenReady(mutateHeaderService(input)){ _ => input.method mustBe HttpMethod.GET }
      }
      it("must preserve http version"){
        val input = request
        whenReady(mutateHeaderService(input)){_ => input.version mustBe HttpVersion.HTTP_1_1 }
      }

      it("must preserve content buffer"){
        val input = request
        val expected = request.content.array()
        whenReady(mutateHeaderService(input)){_ => input.content.array() mustBe expected }
      }
      it("must preserve chunkiness"){
        val input = request
        whenReady(mutateHeaderService(input)){_ => input.isChunked() mustBe false}

      }
    }
  }
}

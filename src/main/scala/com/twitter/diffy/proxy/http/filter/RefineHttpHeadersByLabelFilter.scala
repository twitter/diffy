package com.twitter.diffy.proxy.http.filter

import com.twitter.finagle.Filter
import org.jboss.netty.handler.codec.http.{HttpRequest, HttpResponse}

object RefineHttpHeadersByLabelFilter {

  def rewriteRule(prefix:String):HeaderRule = {
    val prefixLength = prefix.length
    def go(name:String) = {
      if (name.startsWith(prefix)) {
        Some(HeaderEffect.rewrite(name, name.substring(prefixLength)))
      } else None
    }
    go
  }

  def removeRule(prefix:String): HeaderRule = {
    def go(name:String) = {
      if (name.startsWith(prefix)) Some(HeaderEffect.remove(name))
      else None
    }
    go
  }

  def apply(label: String, allLabels: Seq[String]): Filter[HttpRequest, HttpResponse, HttpRequest, HttpResponse] = {
    val (inclusions, exclusion) = allLabels.partition(_.equalsIgnoreCase(label))
    val rules =
      inclusions.map(_ + "_").map(rewriteRule) ++
        exclusion.map(_+ "_").map(removeRule)
    RewriteHttpHeadersFilter(rules)
  }
}

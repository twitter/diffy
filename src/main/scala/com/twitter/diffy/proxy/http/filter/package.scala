package com.twitter.diffy.proxy.http

package object filter {
  type HeaderRule = String => Option[HeaderEffect]
}

package com.twitter.diffy.proxy

import java.net.InetSocketAddress

import com.twitter.util.Duration

case class Settings(
  datacenter: String,
  servicePort:InetSocketAddress,
  candidate: Target,
  primary: Target,
  secondary: Target,
  candidateHeaders: HeaderPairs,
  primaryHeaders: HeaderPairs,
  secondaryHeaders: HeaderPairs,
  protocol: String,
  clientId: String,
  pathToThriftJar: String,
  serviceClass: String,
  serviceName: String,
  apiRoot: String,
  enableThriftMux: Boolean,
  relativeThreshold: Double,
  absoluteThreshold: Double,
  teamEmail: String,
  emailDelay: Duration,
  rootUrl: String,
  allowHttpSideEffects: Boolean,
  excludeHttpHeadersComparison: Boolean,
  skipEmailsWhenNoErrors: Boolean)

case class Target(path: String)

case class HeaderPairs(headerPairs: String)

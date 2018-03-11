package com.twitter.diffy.proxy

import javax.inject.Singleton
import com.google.inject.Provides
import com.twitter.diffy.analysis._
import com.twitter.diffy.lifter.Message
import com.twitter.finagle._
import com.twitter.inject.TwitterModule
import com.twitter.logging.Logger
import com.twitter.util._
import org.jboss.netty.handler.codec.http.HttpRequest

object DifferenceProxyModule extends TwitterModule {
  @Provides
  @Singleton
  def providesDifferenceProxy(
                               settings: Settings,
                               collector: DifferenceCollector,
                               joinedDifferences: JoinedDifferences,
                               analyzer: DifferenceAnalyzer
  ): DifferenceProxy =
    settings.protocol match {
      case "thrift" => ThriftDifferenceProxy(settings, collector, joinedDifferences, analyzer)
      case "http" => SimpleHttpDifferenceProxy(settings, collector, joinedDifferences, analyzer)
      case "https" => SimpleHttpsDifferenceProxy(settings, collector, joinedDifferences, analyzer)
    }
}

object DifferenceProxy {
  object NoResponseException extends Exception("No responses provided by diffy")
  val NoResponseExceptionFuture = Future.exception(NoResponseException)
  val log = Logger(classOf[DifferenceProxy])
}

trait DifferenceProxy {
  import DifferenceProxy._

  type Req
  type Rep
  type Srv <: ClientService[Req, Rep]

  val server: ListeningServer
  val settings: Settings
  var lastReset: Time = Time.now

  def serviceFactory(serverset: String, label: String): Srv

  def liftRequest(req: Req): Future[Message]
  def liftResponse(rep: Try[Rep]): Future[Message]

  // Clients for services
  val candidate = serviceFactory(settings.candidate.path, "candidate")
  val primary   = serviceFactory(settings.primary.path, "primary")
  val secondary = serviceFactory(settings.secondary.path, "secondary")

  val collector: DifferenceCollector

  val joinedDifferences: JoinedDifferences

  val analyzer: DifferenceAnalyzer

  private[this] lazy val multicastHandler =
    new SequentialMulticastService(Seq(primary.client, candidate.client, secondary.client))

  def proxy: Service[Req, Rep] {
    def apply(req: Req): Future[Rep]
  } = new Service[Req, Rep] {
    override def apply(req: Req): Future[Rep] = {
      val rawResponses: Future[Seq[Try[Rep]]] =
        multicastHandler(req) respond {
          case Return(_) => log.debug("success networking")
          case Throw(t) => log.debug(t, "error networking")
        }

      val responses: Future[Seq[Message]] =
        rawResponses flatMap { reps: Seq[Try[Rep]] =>
          Future.collect(reps map liftResponse) respond {
            case Return(rs) =>
              log.debug(s"success lifting ${rs.head.endpoint}")

            case Throw(t) => log.debug(t, "error lifting")
          }
        }

      responses foreach {
        case Seq(primaryResponse, candidateResponse, secondaryResponse) =>
          liftRequest(req) respond {
            case Return(m) =>
              log.debug(s"success lifting request for ${m.endpoint}")

            case Throw(t) => log.debug(t, "error lifting request")
          } foreach { req =>
            analyzer(req, candidateResponse, primaryResponse, secondaryResponse)
          }
      }

      NoResponseExceptionFuture
    }
  }

  def clear(): Future[Unit] = {
    lastReset = Time.now
    analyzer.clear()
  }
}

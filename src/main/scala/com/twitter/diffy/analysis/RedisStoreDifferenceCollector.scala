package com.twitter.diffy.analysis
import akka.actor.ActorSystem
import com.twitter.diffy.compare.Difference
import com.twitter.diffy.thriftscala.DifferenceResult
import com.twitter.logging.Logger
import com.twitter.util.Future
import redis.RedisClient

import scala.util.{Failure, Success}

class RedisStoreDifferenceCollector() extends DifferenceCollector {
  val log = Logger(classOf[RedisStoreDifferenceCollector])
  implicit val system = ActorSystem("RedisCollectorActorSystem")
  implicit val ec = system.dispatcher

  override def create(dr: DifferenceResult): Unit = {
  log.info("Count called in Redis STore Difference colelctor")
      val client = new RedisClient("redis.service.consul",6379,None,None,"RedisStoreDifferenceCollector",None)
      val f = client.ping()

      f.onComplete({
        case Success(str) => log.info(s"ping success response = $str")
        case Failure(e) => log.info("exception connecting to redis")
      })

  }

  override def prefix(field: Field): Future[Iterable[DifferenceResult]] = {
    log.info(s"prefix called with $field")
    Future.apply(Seq.empty[DifferenceResult])
  }

  override def apply(id: Long): Future[DifferenceResult] = {
    Future.exception(new Throwable(s"id $id not found"))
  }

  override def clear(): Future[Unit] = {
    log.info("clear called ")
    Future.Unit
  }
}

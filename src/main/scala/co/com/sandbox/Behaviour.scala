package co.com.sandbox

import cats.data.Reader
import co.com.sandbox.Behaviour.FutureEvent

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait Behaviour[C,D]{
  def behave():Reader[(C,D),FutureEvent]
}

object Behaviour{
  trait Config
  trait Event
  trait CommandData
  type FutureEvent = Future[Seq[Event]]
}


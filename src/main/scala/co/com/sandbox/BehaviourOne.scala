package co.com.sandbox

import cats.data.Reader
import co.com.sandbox.Behaviour._
import co.com.sandbox.BehaviourOneX._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object BehaviourOneX {
  case class ConfigOne(s:String) extends Config
//  case class ConfigOne(s:String)
  case class CommandOneData(a:Int, b:Int) extends CommandData
//  case class CommandOneData(a:Int, b:Int)
  case class EventOne(data:String) extends Event
//  case class EventOne(data:String)
}

object BehaviourDefault extends Behaviour[Config, CommandData] {

  def behave: Reader[(Config, CommandData), FutureEvent] = Reader{
    t =>
      Future{
        Seq()
      }
  }
}

object BehaviourOne extends Behaviour[ConfigOne, CommandOneData] {

  def behave: Reader[(ConfigOne, CommandOneData), FutureEvent] = Reader{
    t =>
      Future{
        Seq(EventOne(s"Evento 1 con conf: ${t._1}"))
      }
  }
}
package co.com.sandbox

import co.com.sandbox.Behaviour.{CommandData, Config, Event}
import co.com.sandbox.BehaviourOneX._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

trait Auxiliar {
  def configs: Map[String, (Config, String => CommandData)]
}
abstract class ControllerBase[C,D]() extends Auxiliar{

  def ejecutar(nombreComando:String, commandDataJson:String) = {

    val t = configs(nombreComando)
    println(s"El t encontrado es $t")
    val c = t._1
    println(s"El c encontrado es $c")
    println(s"El f encontrado es ${t._2}")
    val d = t._2(commandDataJson)
    println(s"El d evaluado es ${d}")

    (c,d) match {
      case t:(ConfigOne, CommandOneData) => foo(t._1,t._2)
    }

  }

  private[ControllerBase] def foo[C,D](c:C, d:D)(implicit b: Behaviour[C,D]) : Unit = {
    val r = b.behave().run((c,d))
    val r1 = Await.result(r, Duration.Inf)
    println(s"El resultado r fue: $r1")
    r.map(se => se.foreach(publicarEvento))
  }

  private[ControllerBase] def publicarEvento(e:Event) = {
    println(s"Publicando evento $e")
  }

}

import cats.Id
import cats.data.Reader
import org.scalatest._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

class SandboxSuite extends FunSuite {

  test("Smoke test"){
    assert(true)
  }


  test("Currying vs Partial Functions"){

    def foo(a:Int)(b:Int): Int = {
      a + b
    }

    def bar = new PartialFunction[Int, Int] {
      override def apply(a:Int) = a + 1
      override def isDefinedAt(a: Int) = a < 1000
    }
1
    val r = foo(1){
      2
    }

    assert(r == 3)

  }

  test("Execution contexts"){

    import java.util.concurrent.Executors
    import scala.concurrent._
    import scala.concurrent.duration.Duration

    def log(i:String) = {
      println(s"${Thread.currentThread().getName} $i")

    }

    object ecs {
      val ecForBD = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
      val ecForBar = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
    }

    var cont = 0

    def iraBD(i:Int) = {

      import ecs._
      implicit val ecFoo = ecForBD


      log(s"A punto de lanzar hilo en iraBD($i)")
      val res = Future{
        log(s"Ejecutando iraBD($i)")
        //Thread.sleep(1000)
        i
      }

      cont = cont + 1

      if(cont > 1000) throw new Exception("No acepto mas de 1000 peticiones seguidas")

      res


    }

    def bar(i:Int): Future[Int] = {
      import ecs._
//      implicit val ec = ecForBar
      import scala.concurrent.ExecutionContext.Implicits.global
      val t1 = System.nanoTime()
      val r: Future[Int] = iraBD(i)
      val t2 = System.nanoTime()
      val elapsed = t2 - t1
      log(s"Elapsed for foo($i): $elapsed")
      r.map { x =>
        log(s"iraBDs termina con i: $i")
        x
      }
    }

    (1 to 10000).foreach {
      bar(_)
    }

    log("I'm done")

    assert(true)

  }

  test("El nino quiere llorar"){
    @tailrec
    def loop: Int = loop

    def first(x: => Int, y: => Int): Int = x
    //def first(x: Int, y: Int): Int = x

    first(10, loop)

  }

}

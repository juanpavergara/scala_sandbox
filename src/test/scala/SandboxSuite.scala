import java.util.concurrent.Future

import cats.Id
import cats.data.Reader
import org.scalatest._

import scala.concurrent.Future

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

}

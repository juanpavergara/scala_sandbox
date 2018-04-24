import org.scalatest.FunSuite

import scala.annotation.tailrec

class TailRecSuite extends FunSuite{
  test("El nino quiere llorar 1"){
    @tailrec
    def loop: Int = loop

    def first(x: => Int, y: => Int): Int = x

    val r = first(10, loop)

    assert(r==10)

  }

  test("El nino quiere llorar 2"){
    @tailrec
    def loop: Int = loop

    def first(x: Int, y: Int): Int = x

    val r = first(10, loop)

    assert(r==10)

  }

}

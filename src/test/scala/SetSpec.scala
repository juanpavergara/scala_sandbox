import org.scalatest._
import prop._
import scala.collection.immutable._

class SetSpec extends FunSuite with TableDrivenPropertyChecks with GeneratorDrivenPropertyChecks with Matchers{

  class Fraction(n: Int, d: Int) {

    require(d != 0)
    require(d != Integer.MIN_VALUE)
    require(n != Integer.MIN_VALUE)

    val numer = if (d < 0) -1 * n else n
    val denom = d.abs

    override def toString = numer + " / " + denom
  }

  val examples =
    Table(
      "set",
      BitSet.empty,
      HashSet.empty[Int],
      TreeSet.empty[Int]
    )

  val triples = Table(
    ("a", "b", "c"),
    (1,1,1),
    (2,2,2),
    (3,3,3)
  )

  test("Table - an empty Set should have size 0") {
    forAll(examples) { set =>
      assert(set.size == 0)
    }
  }

  test("Table - Invoking head on an empty set should produce NoSuchElementException") {
    forAll(examples) { set =>
      a [NoSuchElementException] should be thrownBy { set.head }

    }
  }

  test("Table - con tuplas"){
    forAll(triples) {
      (a,b,c) =>
        assert(a == b)
        assert(b == c)
    }
  }

  test("Generator - Verificador de fracciones") {
    forAll { (n: Int, d: Int) =>

      whenever(d != 0 &&
        d != Integer.MIN_VALUE &&
        n != Integer.MIN_VALUE) {

        val f = new Fraction(n, d)

        if (n < 0 && d < 0 || n > 0 && d > 0)
          assert(f.numer > 0)
        else if (n != 0)
          assert(f.numer < 0)
        else
          assert(f.numer == 0)

        assert(f.denom > 0)
      }
    }
  }
}
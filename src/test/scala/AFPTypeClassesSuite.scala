import org.scalatest.FunSuite

class AFPTypeClassesSuite extends FunSuite {

  test("Ejercicio type class"){

    case class ADT1(s: String, i:Int)

    case class ADT2(s: String, i:Int)

    trait Conversor[T]{
      def toADT(t:(String,Int)):T
    }

    object Conversor{
      implicit object ConversorADT1 extends Conversor[ADT1]{
        def toADT(t:(String, Int)) = ADT1(t._1, t._2)
      }

      implicit object ConversorADT2 extends Conversor[ADT2]{
        def toADT(t:(String, Int)) = ADT2(t._1, t._2)
      }
    }

    def toADT[T](t:(String, Int))(implicit c: Conversor[T]):T = {
        c.toADT(t)
    }

    val adt1: ADT1 = toADT[ADT1](("a", 1))
    val adt2: ADT2 = toADT[ADT2](("a", 1))

    assert(adt1.s == "a")
    assert(adt1.i == 1)
    assert(adt2.s == "a")
    assert(adt2.i == 1)

  }
}

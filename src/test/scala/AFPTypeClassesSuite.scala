import org.scalatest.FunSuite

class AFPTypeClassesSuite extends FunSuite {

  test("Ejercicio type class"){

    case class ADT1(s: String, i:Int)

    case class ADT2(s: String, i:Int)

    trait Conversor[I, T]{
      def toADT(t:I):T
    }

    object Conversor{
      implicit object ConversorADT1 extends Conversor[Tuple2[String,Int],ADT1]{
        def toADT(t:(String, Int)) = ADT1(t._1, t._2)
      }

      implicit object ConversorADT2 extends Conversor[Tuple2[String,Int],ADT2]{
        def toADT(t:(String, Int)) = ADT2(t._1, t._2)
      }
    }

    def toADT[I,T](t:I)(implicit c: Conversor[I,T]):T = {
        c.toADT(t)
    }

    val adt1: ADT1 = toADT[Tuple2[String,Int], ADT1](("a", 1))
    val adt2: ADT2 = toADT[Tuple2[String,Int], ADT2](("a", 1))

    assert(adt1.s == "a")
    assert(adt1.i == 1)
    assert(adt2.s == "a")
    assert(adt2.i == 1)

  }
}

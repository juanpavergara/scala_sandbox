import org.scalatest.FunSuite

class AFPTypeClassesSuite extends FunSuite {

  test("Ejercicio type class"){

    case class ADT1(s: String, i:Int)
    case class ADT2(s: String, i:Int)

    trait Conversor[I, T]{
      def toADT(t:I):T
    }

    trait ConversorSyntax{
      implicit class ConversorOps[I](i:I){
        def toADT[T](implicit c:Conversor[I,T]): T = c.toADT(i)
      }
    }

    trait ConversorInstances extends ConversorSyntax{
      implicit def ConversorADT1 = new Conversor[(String,Int),ADT1]{
         def toADT(t:(String, Int)) = ADT1(t._1, t._2)
      }

      implicit def ConversorADT2 = new Conversor[(String,Int),ADT2]{
        def toADT(t:(String, Int)) = ADT2(t._1, t._2)
      }

    }

    object conversores extends ConversorInstances

    import conversores.ConversorADT1
    import conversores.ConversorADT2
    import conversores.ConversorOps
    val adt1 = ("5", 5).toADT[ADT1]
    val adt2 = ("5", 5).toADT[ADT2]

    assert(adt1.s == "5")
    assert(adt1.i == 5)
  }
}

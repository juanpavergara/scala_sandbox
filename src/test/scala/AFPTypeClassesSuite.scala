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

  test("Ejercicio type class con subtipado"){

    trait MySuperType
    case class MySubtypeOne(name:String) extends MySuperType
    case class MySubtypeTwo(value:Int) extends MySuperType

    trait MyTypeClass[T<:MySuperType]{
      def myOp():T
    }

    trait MyTypeClassInstances extends MyTypeClassSyntax{

      implicit def MyFirstMember = new MyTypeClass[MySubtypeOne] {
        def myOp() = MySubtypeOne("fixed")
      }

      implicit def MySecondMember = new MyTypeClass[MySubtypeTwo] {
        def myOp() = MySubtypeTwo(2)
      }

    }

    trait MyTypeClassSyntax   {
      implicit class MyTypeClassOps[T<:MySuperType](i:T){
         def myOp(implicit c:MyTypeClass[T]): T = c.myOp
      }
    }


    def produceT(i:Int): MySuperType = i%2 match {
      case 0 => MySubtypeOne("one")
      case _ => MySubtypeTwo(2)
    }

    object o extends MyTypeClassInstances

    def bar[T<:MySuperType](t:T) = {

      /*
      Como lograr llamar a foo sin un pattern match?
       */

      import o._

      t match {
        case x: MySubtypeOne => {
          assert(x.myOp ==MySubtypeOne("fixed"))
        }
        case y: MySubtypeTwo => {
          assert(y.myOp == MySubtypeTwo(2))
        }
      }

      assertDoesNotCompile("foo(t)")

    }

    bar(produceT(2))
    bar(produceT(1))
  }
}

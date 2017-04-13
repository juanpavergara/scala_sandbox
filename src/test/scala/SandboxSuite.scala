import java.util.concurrent.Future

import cats.Id
import cats.data.Reader
import org.scalatest._

import scala.concurrent.Future

class SandboxSuite extends FunSuite {

  test("Smoke test"){
    assert(true)
  }

  test("Type Class design pattern"){

    /*
    Object para hacer disponible al trait interno el cual es el Type Class
     */
    object MyTypeClassObject{

      /*
      Este trait es el Type Class *per se*
      Es parametrizado. La idea es que las funciones del trait estén parametrizadas a los tipos
      a los cuales está parametrizado el trait.
      Preferiblemente se debe evitar tener tipos concretos en las firmas de las funciones del Type Class
       */
      trait MyTypeClass[T]{
        def myOp(i:T):T
      }
    }

    import MyTypeClassObject._

    /*
    Ahora hacemos un *miembro* del Type Class
    Aqui se materializa el polimorfismo ad-hoc pues estamos implementando myOp
    específicamente para el tipo String (en este ejemplo).

    Se hace implicit para que la conversión la haga el compilador y no se haga explícitamente.
    Es posible que esto se vea más claro a la hora de usarlo.
     */
    implicit object MyTypeClassMemberForString extends MyTypeClass[String] {
      def myOp(i:String) = s"Hello $i"
    }

    /*
    Ahora definimos una funcion cualquiera foo en la cual es importante destacar lo siguiente:
    1. Es parametrizada en T. Se busca que sea genérica via parametrizacion.
    2. Depende de un MyTypeClass para internamente valerse de myOp para su comportamiento.
    3. No depende cómo tc hace myOp. Solo depende de lo que promete myOp en el Type Class.
     */

    def foo[T](t:T)(implicit tc: MyTypeClass[T]) : T = {
      tc.myOp(t)
    }

    /*
    Se puede definir lo mismo pero con otra sintaxis como se hace a continuacion con la funcion bar
    (hace exactamente lo mismo que foo). Importante tener en cuenta:

    1. El tipo parametrizado es T:MyTypeClass y no T solito. Esto es una alternativa
    que solo funciona para Type Classes de un solo tipo parametrizado.
    2. Ya no hay segundo parametri implicit y se reemplaza por el implicitly en el cuerpo
    de la funcion al evaluar myOp
     */
    def bar[T:MyTypeClass](t:T) : T = {
      implicitly[MyTypeClass[T]].myOp(t)
    }



    /*
    La siguiente evaluacion funciona porque existe un implicit object MyTypeClassMemberForString
     */
    assert(foo("Juancho") == "Hello Juancho")
    assert(bar("Juancho") == "Hello Juancho")
    /*
    La siguiente evaluacion no compila porque no existe definicion implicita de un MyTypeClass[Int]
     */
    assertDoesNotCompile("foo(1) == \"Hello Juancho\"")
  }


  test("Subtyped type class"){
    trait MySuperType
    case class MySubtypeOne(name:String) extends MySuperType
    case class MySubtypeTwo(value:Int) extends MySuperType

    trait MyTypeClass[A<:MySuperType]{
      def myOp():A
    }

    implicit object MyFirstMember extends MyTypeClass[MySubtypeOne]{
      def myOp() = MySubtypeOne("fixed")
    }

    implicit object MySecondMember extends MyTypeClass[MySubtypeTwo]{
      def myOp() = MySubtypeTwo(2)
    }


    def foo[T<:MySuperType](t:T)(implicit tc: MyTypeClass[T]) : T = {
      tc.myOp
    }

    val r1 = foo(MySubtypeOne("first"))
    val r2 = foo(MySubtypeTwo(1))

    assert(r1 == MySubtypeOne("fixed"))
    assert(r2 == MySubtypeTwo(2))


  }

  test("Subtyped type class 2"){
    trait MySuperType
    case class MySubtypeOne(name:String) extends MySuperType
    case class MySubtypeTwo(value:Int) extends MySuperType

    trait MyTypeClass[A<:MySuperType]{
      def myOp():A
    }

    implicit object MyFirstMember extends MyTypeClass[MySubtypeOne]{
      def myOp() = MySubtypeOne("fixed")
    }

    implicit object MySecondMember extends MyTypeClass[MySubtypeTwo]{
      def myOp() = MySubtypeTwo(2)
    }


    def foo[T<:MySuperType](t:T)(implicit tc: MyTypeClass[T]) : T = {
      tc.myOp
    }

    def produceT(i:Int): MySuperType = i%2 match {
      case 0 => MySubtypeOne("one")
      case _ => MySubtypeTwo(2)
    }

    def bar[T<:MySuperType](t:T) = {

      /*
      Como lograr llamar a foo sin un pattern match?
       */
      t match {
        case x: MySubtypeOne => {
          assert(foo(x)==MySubtypeOne("fixed"))
        }
        case y: MySubtypeTwo => {
          assert(foo(y)==MySubtypeTwo(2))
        }
      }

    }

    val t = produceT(2)

    bar(t)


  }

}

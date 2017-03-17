import org.scalatest._

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
    La siguiente evaluacion funciona porque existe un implicit object MyTypeClassMemberForString
     */
    assert(foo("Juancho") == "Hello Juancho")
    /*
    La siguiente evaluacion no compila porque no existe definicion implicita de un MyTypeClass[Int]
     */
    assertDoesNotCompile("foo(1) == \"Hello Juancho\"")
  }
}

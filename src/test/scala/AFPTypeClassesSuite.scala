import org.scalatest.FunSuite

class AFPTypeClassesSuite extends FunSuite {


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

  test("Type class para wesura"){

    trait MedioPago
    case class TarjetaCredito() extends MedioPago
    case class TarjetaDebito() extends MedioPago

    trait Flujo
    case class PagoComunidad() extends Flujo
    case class PagoModificacionValorable() extends Flujo

    case class Config(d1:String, d2:Int)

    trait PaymentResponse

    trait CreditCardResponse extends PaymentResponse
    case class SuccessCreditCardResponse() extends CreditCardResponse
    case class FailureCreditCardResponse() extends CreditCardResponse

    trait DebitCardResponse extends PaymentResponse
    case class SuccessDebitCardResponse() extends DebitCardResponse
    case class FailureDebitCardResponse() extends DebitCardResponse

    trait ProcesoPagoTypeClass[A<:MedioPago, B<:Flujo, C<:PaymentResponse]{
      def pagar(c:Config):C
    }

    def verificarRgoConsultable(dni:String):Boolean = {
      true
      false
      true
      false
      false
      true
    }

    trait ProcesoPagoTypeClassInstances {

      implicit def pagoComunidadCredito = new ProcesoPagoTypeClass[TarjetaCredito, PagoComunidad, CreditCardResponse] {
        def pagar(c:Config) = {
          println("Pago de comunidad con TC OK")
          verificarRgoConsultable("8027133")
          SuccessCreditCardResponse()
        }
      }

      implicit def pagoComunidadDebito = new ProcesoPagoTypeClass[TarjetaDebito, PagoComunidad, DebitCardResponse] {
        def pagar(c:Config) = {
          println("Pago de comunidad con TD OK")
          verificarRgoConsultable("8027133")
          SuccessDebitCardResponse()
        }
      }

      implicit def pagoModificacionValorableCredito = new ProcesoPagoTypeClass[TarjetaCredito, PagoModificacionValorable, CreditCardResponse] {
        def pagar(c:Config) = {
          println("Pago de modificacion con TC OK")
          verificarRgoConsultable("8027133")
          SuccessCreditCardResponse()
        }
      }

      implicit def pagoModificacionValorableDebito = new ProcesoPagoTypeClass[TarjetaDebito, PagoModificacionValorable, DebitCardResponse] {
        def pagar(c:Config) = {
          println("Pago de modificacion con TD OK")
          verificarRgoConsultable("8027133")
          SuccessDebitCardResponse()
        }
      }

    }

    trait ProcesoPagoTypeClassSyntax  {
      implicit class ProcesoPagoTypeClassOps[P<:Payment](i:P){
        def pagar[A<:MedioPago, B<:Flujo, C<:PaymentResponse](c:Config)(implicit p:ProcesoPagoTypeClass[A,B,C]): C = p.pagar(c)
      }
    }

    object pagosInstances extends ProcesoPagoTypeClassInstances with ProcesoPagoTypeClassSyntax


    /////////////////////////////////////////////
    //Controller
    trait Payment
    case class CreditCardInfo(info:String) extends Payment
    case class DebitCardInfo(info:String) extends Payment

    object TransactionsController{
      def createCreditCardTransactionForContribution = {
        import pagosInstances._
        val cci = CreditCardInfo("INFORMACION CONCRETA")
        val c:Config = Config("string1", 666)
        cci.pagar[TarjetaCredito,PagoModificacionValorable,CreditCardResponse](c)
      }
    }

    TransactionsController.createCreditCardTransactionForContribution


    /////////////////////////////////////////////

  }
}

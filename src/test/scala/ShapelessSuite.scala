import org.scalatest.FunSuite
import shapeless.Generic.Aux
import shapeless._

class ShapelessSuite extends FunSuite {

  test("Generic for product types") {
    case class IceCream(name:String, numCherries:Int, inCone:Boolean)
    case class Employee(name:String, number:Int, manager:Boolean)

    val genIceCream = Generic[IceCream]

    val iceCream = IceCream("Banana", 0, false)
    //Tomar un IceCream concreto y llevarlo _to_ una representacion generica
    //Esto genera un HList con los campos del producto (case class)
    val genRepr = genIceCream.to(iceCream)

    // Como IceCream y Employee tienen la misma estructura entonces se puede lograr
    // llegar a algo concreto (un Employee concreto) a partir de una representacion generica
    // si esta representacion tiene la misma estructura (String, Int, Boolean)
    val employee = Generic[Employee].from(genRepr)

    assert(!employee.manager)
  }

  test("Generic for coproducts (sum types)"){
    sealed trait Shape
    case class Rectangle(a:Int, b:Int) extends Shape
    case class Circle(r:Int) extends Shape

    // Shapeless representa coproductos con :+: y CNil
    // Este operador significa algo como: el tipo genRepType es un Rectangle o un Circle ... disyuntivamente.
    type genRepType = Rectangle :+: Circle :+: CNil

    // El siguiente genRep es el mismo typo de la linea previa pero con la ayuda de Generic
    val genRep = Generic[Shape]

    val a = genRep.to(Rectangle(1,1))
    val b = genRep.to(Circle(1))

    assert(a == Inr(Inl(Rectangle(1,1))))
    assert(b == Inl(Circle(1)))

    // Se pueden crear instancias de Coproduct mezclando Inl e Inr.
    // Y siempre debe haber un solo Inl.
    // Y no se puede construir un Coproduct solo con Inr
    val x = Inl(Circle)

    assertDoesNotCompile("val y: Coproduct = Inr(Rectangle)")

    // Si tuvieramos habitantes de tipo de igual estructura se podria hacer un intercambio
    // mediante sus representaciones genericas


  }
}
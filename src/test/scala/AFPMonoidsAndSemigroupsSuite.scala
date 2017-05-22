import cats._

import org.scalatest.FunSuite

class AFPMonoidsAndSemigroupsSuite extends FunSuite{

  test("Ejercicio de Monoids y Semigroups"){
    case class Efecto(valor:Int)

    implicit val effectMonoid = new Monoid[Efecto]{
      override def empty = Efecto(0)
      override def combine(x: Efecto, y: Efecto) = Efecto(x.valor+y.valor)
    }

    implicit class EfectoOps[A](s:Seq[A]) {
      def aplicar(implicit m: Monoid[A]) = s.fold(m.empty)(m.combine)
    }

    //TODO: llegar a dos listas de efectos dependiendo de la población en la que se inserte la nueva especie
    val efectos =  (Efecto(1)::Efecto(-1)::Nil)
    val nuevoEquilibrio: Efecto = efectos.aplicar

    assert(nuevoEquilibrio==Efecto(0))
  }

  test("Insertar especies"){
    trait Poblacion
    case class Herbivora() extends Poblacion
    case class Carnivora() extends Poblacion
    trait Especie
    case class Especie1() extends Especie
    case class Efecto(valor:Int)

    trait TC[A<:Poblacion, B<:Especie]{
      def insertar(e:B):Efecto
    }

    trait TCInstances {
      implicit def insertarEspecie1EnHerb = new TC[Herbivora, Especie1] {
        def insertar(e: Especie1) = Efecto(1)
      }

      implicit def insertarEspecie1EnCarn = new TC[Carnivora, Especie1] {
        def insertar(e: Especie1) = Efecto(0)
      }
    }

    trait TCSyntax {
      implicit class TCOps[P<:Poblacion, E<:Especie](p:P) {
        def insertar(e:E)(implicit c:TC[P,E]):Efecto = c.insertar(e)
      }
    }

    object o extends TCInstances with TCSyntax

    import o._

    val e1 = Carnivora().insertar(Especie1()) :: Carnivora().insertar(Especie1())  ::  Nil
    val e2 = Herbivora().insertar(Especie1()) :: Herbivora().insertar(Especie1())  :: Nil


    implicit val effectMonoid = new Monoid[Efecto]{
      override def empty = Efecto(0)
      override def combine(x: Efecto, y: Efecto) = Efecto(x.valor+y.valor)
    }

    implicit class EfectoOps[A](s:Seq[A]) {
      def aplicar(implicit m: Monoid[A]) = s.fold(m.empty)(m.combine)
    }

    assert(e1.aplicar==Efecto(0))
    assert(e2.aplicar==Efecto(2))

  }
}

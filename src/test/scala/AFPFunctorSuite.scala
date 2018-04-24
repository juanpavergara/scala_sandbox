import org.scalatest.FunSuite
import cats._

class AFPFunctorSuite extends FunSuite{

  test("Smoke test"){
    assert(true)
  }

  test("Basic functor"){
    case class MyADT[A](i:A)
    implicit val myFunctor = new Functor[MyADT]{
      override def map[A, B](fa: MyADT[A])(f: (A) => B) = MyADT(f(fa.i))
    }

    implicit class MyADTOps[F[_],A,B](i:F[A]){
      def fmap(f: A=>B)(implicit functor: Functor[F]) = functor.map(i)(f)
    }

    assert(MyADT(0).fmap(x=>x+1)==MyADT(1))
  }

  test("First attempt"){
    assert(true)
  }



}

import java.util.concurrent.atomic.AtomicReference

import com.twitter.util.Activity.{Ok, State}
import com.twitter.util.{Activity, Closable, Var, Witness}
import org.scalatest.FunSuite

import com.twitter.util.Future

class TwitterUtilSuite extends FunSuite {

  test("Smoke test"){
    assert(true)
  }

  test("Activity#flatMap") {
    val v = Var(Activity.Pending: Activity.State[Int])
    val ref = new AtomicReference[Seq[Activity.State[Int]]]

    val act = Activity(v) flatMap {
      case i if i % 2 == 0 => Activity.value(-i)
      case i => Activity.value(i)
    }

    act.states.build.register(Witness(ref))

    assert(ref.get == Seq(Activity.Pending))

    v() = Activity.Ok(1)
    assert(ref.get == Seq(Activity.Pending, Activity.Ok(1)))

    v() = Activity.Ok(2)
    assert(ref.get == Seq(Activity.Pending, Activity.Ok(1), Activity.Ok(-2)))

    println(s"act: $act")
    println(s"act.states: ${act.states}")
    println(s"ref.get: ${ref.get}")


  }

  test("Activity#stateupdate") {
    val v = Var(Activity.Pending: Activity.State[Int])
    val ref = new AtomicReference[Seq[Activity.State[Int]]]

    val act = Activity(Var.async(Activity.Pending: Activity.State[Int]) { state =>
      state.update(Activity.Ok(1))
      state.update(Activity.Ok(2))
      state.update(Activity.Ok(3))

      Closable.make { _ =>
        Future.Unit
      }
    })

    act.states.build.register(Witness(ref))

    assert(ref.get == Seq(Activity.Ok(3)))


    println(s"act: $act")
    println(s"act.states: ${act.states}")
    println(s"ref.get: ${ref.get}")


  }

}

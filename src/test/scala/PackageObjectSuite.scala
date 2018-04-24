import org.scalatest.FunSuite
import foo.bar.QuxClient._
import foo.bar.QuxClient

class PackageObjectSuite extends FunSuite{
  test("Smoke test"){
    assert(true)
  }
  test("Using qux from package object"){
    val res1 = QuxClient.useQux(1)
    val res2 = useQux(1)
    assert(res1 === res2)
    assert(res1 == 1)
  }

}

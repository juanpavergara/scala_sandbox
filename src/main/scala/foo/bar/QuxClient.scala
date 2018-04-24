package foo.bar


sealed trait QuxClient {
  def useQux(i:Int) = qux(i)
}

object QuxClient extends QuxClient

import org.scalatest.funsuite.AnyFunSuite

class Test1 extends AnyFunSuite {

  test(s"Result for the empty list") {
    assert(wystąpienia(Nil) === List())
  }

  val arg1 = List(1, 2, 3)
  test(s"Result for $arg1") {
    assert(wystąpienia(arg1) === List((1, 1), (2, 1), (3, 1)))
  }

  val arg2 = List(1, 1, 1)
  test(s"Result for $arg2") {
    assert(wystąpienia(arg2) === List((1, 3)))
  }

  val arg3 = List(1, 2, 3, 3, 2, 1)
  test(s"Result for $arg3") {
    assert(wystąpienia(arg3) === List((1, 2), (2, 2), (3, 2)))
  }

  val arg4 = List(3, 2, 3, 3, 2, 1)
  test(s"Result for $arg4") {
    assert(wystąpienia(arg4) === List((3, 3), (2, 2), (1, 1)))
  }

}

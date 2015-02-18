package oscar.lcg.core

import oscar.lcg.testUtils._
import scala.util.Random

class LiteralSuite extends TestSuite {

  test("x.opposite.opposite should be x.") {
    val x = new Literal(0, "x", "!x")
    val xOp = x.opposite
    assert(xOp.opposite == x)
    assert(x.opposite.opposite.opposite == xOp)
  }

  test("-x should be x.opposite.") {
    val x = new Literal(0, "x", "!x")
    assert(-x == x.opposite)
  }

  test("dividing the id of a literal by two should yield its varId.") {
    val rng = new Random(0)
    for (_ <- 1 to 1000) {
      val varId = rng.nextInt(1000)
      val x = new Literal(varId, "x")
      val xOp = x.opposite
      assert(x.id / 2 == varId)
      assert(xOp.id / 2 == varId)
    }
  }

  test("id of a signed literal should always be the id of the unsigned + 1.") {
    val rng = new Random(0)
    for (_ <- 1 to 1000) {
      val varId = rng.nextInt(1000)
      val x = new Literal(varId, "x")
      val xOp = x.opposite
      assert(x.id + 1 == xOp.id)
    }
  }
}
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._

class TestGCC extends FunSuite with ShouldMatchers {
  val rand = new scala.util.Random(0)

  def randomDom(n: Int): Array[Array[Int]] = {
    val low = Array.tabulate(n)(i => rand.nextInt(2))
    val up = Array.tabulate(n)(i => low(i) + rand.nextInt(2))
    Array(low, up)
  }

  def randomOcc(n: Int): Array[Array[Int]] = {
    val low = Array.tabulate(n)(i => rand.nextInt(1))
    val up = Array.tabulate(n)(i => low(i) + rand.nextInt(3))
    Array(low, up)
  }

  /**
   * return the number of sol of the constraints
   */
  def nbSolution(randomDom: Array[Array[Int]], randomOcc: Array[Array[Int]], gccvar: Boolean): Int = {
    val cp = CPSolver()
    val x: Array[CPVarInt] = Array.tabulate(randomDom(0).size)(i => CPVarInt(randomDom(0)(i) to randomDom(1)(i))(cp))
    val o: Array[CPVarInt] = Array.tabulate(randomOcc(0).size)(i => CPVarInt(randomOcc(0)(i) to randomOcc(1)(i))(cp))

    var nb = 0

    if (gccvar) {
      cp.post(new oscar.cp.constraints.GCCVar(x, -1, o));
    } else {
      cp.post(new oscar.cp.constraints.SoftGCC(x, -1, randomOcc(0), randomOcc(1), CPVarInt(0)(cp)));
    }
    if (cp.isFailed()) {
      return -1;
    }

    cp.solve exploration {
      cp.binary(x)
      if (gccvar) o.forall(_.isBound) should be(true)
      nb += 1
    } run ()
    nb
  }

  test("GCC1") {
    for (i <- 0 until 150) {
      val randDom = randomDom(3)
      val randOcc = randomOcc(4)
      nbSolution(randDom, randOcc, true) should be(nbSolution(randDom, randOcc, false))
    }
  }

  test("GCC2") {

    val cp = CPSolver()

    val x = Array.fill(10)(CPVarInt(0 to 10)(cp))

    for (i <- 0 until 2; v <- 0 until 5) {
      cp.post(x(i * 5 + v) == v)
    }

    val o = Array.fill(10)(CPVarInt(0 to 10)(cp))

    cp.post(new oscar.cp.constraints.GCCVar(x, 0, o));

    cp.isFailed() should be(false)

    for (i <- 0 until o.size) {
      o(i).isBound should be(true)
      if (i < 5) o(i).value should be(2)
      else o(i).value should be(0)
    }
  }

}

package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.algo.search.IDSSearchController
import oscar.cp.modeling._

class TestSearch extends FunSuite with ShouldMatchers {

  test("ids search, bug #36") {
    val cp = new CPSolver()
    val x = Array(CPVarInt(0)(cp))
    var nbSol = 0
    cp.sc = new IDSSearchController(cp, 4)
    cp.minimize(x(0)) subjectTo () exploration {
      cp.binaryFirstFail(x)
      println(x.mkString(","))
      nbSol += 1
    } run ()
    nbSol should be(1)
  }

  test("explocompleted") {
    val cp = CPSolver()
    val v = Array.tabulate(3)(i => false)
    var nb = 0
    cp.exploration {
      cp.branch { v(0) = true } { v(0) = false }
      cp.branch { v(1) = true } { v(1) = false }
      cp.branch { v(2) = true } { v(2) = false }
      println(v.mkString(","))
      nb += 1
    }

    cp.run(nbSolMax = 3)
    cp.explorationCompleted should be(false)
    cp.run()
    cp.explorationCompleted should be(true)
    cp.run(nbSolMax = 3)
    cp.explorationCompleted should be(false)
    cp.run()
    cp.explorationCompleted should be(true)
    cp.run(failureLimit = 3)
    cp.explorationCompleted should be(false)
  }

  test("timelimit") {
    val cp = CPSolver()
    val x = Array.fill(20)(CPVarInt(0 to 1)(cp))
    var nb = 0
    var t0 = System.currentTimeMillis()
    cp.solve subjectTo {

    } exploration {
      cp.binary(x)
      //println(x.mkString(","))
      nb += 1
    }
    cp.run(timeLimit = 1)
    cp.run(timeLimit = 1)
    val time: Int = ((System.currentTimeMillis() - t0) / 1000).toInt
    time should be >= (2)
    cp.explorationCompleted should be(false)
    time should be <= (3)
    cp.explorationCompleted should be(false)
  }

  test("optimize") {

    val cp = new CPSolver()
    val x = CPVarInt(Array(1, 5, 9, 10))(cp)
    cp.minimize(x)
    cp.exploration {
      cp.binary(Array(x), _.max)
      println(x)

    }

  }

  test("test 2 dfs") {

    val cp = CPSolver()
    val x = Array.fill(2)(CPVarInt(1 to 2)(cp))
    val y = Array.fill(2)(CPVarInt(1 to 2)(cp))

    def dom(x: CPVarInt) = (x.min to x.max).filter(x.hasValue(_))

    var nbSol = 0
    cp.exploration {
      while (!allBounds(x)) {
        val i = x.indices.find(!x(_).isBound).get
        cp.branchAll(dom(x(i)))(v => cp.post(x(i) == v))
        cp.branchAll(dom(y(i)))(v => cp.post(y(i) == v))
      }
      nbSol += 1
    } run ()
    nbSol should equal(16)
  }
}

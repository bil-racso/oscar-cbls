package scampi.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import scampi.cp.constraints._
import scampi.cp.core._
import scampi.cp.search._
import scampi.cp.modeling._

import org.scalacheck._

class TestGCCFWC extends FunSuite with ShouldMatchers {
	import TestGCCFWC._
  
  test("Test 1: too few variables to satisfy the lower bounds of CC") {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 2, 0, 2)
    val Up = Array(3, 2, 3, 2)

    var nbSol = 0;
    cp.solveAll subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } exploration {
      cp.binaryFirstFail(x)
      nbSol += 1
    }

    nbSol should be (0)
  }
  
    test("Test 2: too many variables to satisfy the upper bounds of CC") {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0, 0)
    val Up = Array(2, 0, 0, 0)

    var nbSol = 0;
    cp.solveAll subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } exploration {
      cp.binaryFirstFail(x)
      nbSol += 1
    }

    nbSol should be (0)
  }
    
  test("Test 3: Variation of 2 that should have some solutions") {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0)
    val Up = Array(2, 0, 0)

    var nbSol = 0;
    cp.solveAll subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } exploration {
      cp.binaryFirstFail(x)
      nbSol += 1
    }

    nbSol should be > 0
  }
  
  test("Test 4: verifying a solution") {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    // T4
    val minVal = 0
    val Low = Array(0, 1, 0, 2)
    val Up = Array(3, 2, 3, 2)

    cp.add(new GCCFWC(x, minVal, Low, Up))

    val sol = Array[Int](1, 3, 3)
    for (i <- 0 until x.length) {
      x(i).getValue() should be(sol(i))
    }
  }

  test("Test 5: counting solutions") {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0, 2)
    val Up = Array(0, 2, 3, 2)

    var nbSol = 0;
    cp.solveAll subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } exploration {
      cp.binaryFirstFail(x)
      println((x map (_.getValue())).mkString(";"))
      nbSol += 1
    }

    nbSol should be (2)
  }

  test("Test 6: counting solutions") {
    val cp = new CPSolver()
    var x1 = new CPVarInt(cp, 0, 2)
    var x2 = new CPVarInt(cp, 1, 3)
    var x3 = new CPVarInt(cp, 0, 3)
    val x = Array(x1, x2, x3)

    val minVal = 0
    val Low = Array(0, 0, 0, 0)
    val Up = Array(1, 1, 1, 1)

    var nbSol = 0;
    cp.solveAll subjectTo {
      cp.add(new GCCFWC(x, minVal, Low, Up))
    } exploration {
      cp.binaryFirstFail(x)
      println((x map (_.getValue())).mkString(";"))
      nbSol += 1
    }

    nbSol should be (14)

  }

  test("Test 7: counting solution of random instances") {
    println("Test 7")
    var nbSol = 0
    var nbSolExhaustiveSearch = 0
    
    val cp = new CPSolver()
    
    // Generate the constraint and solve
    val GCC = GCCGen(cp)
    cp.solveAll subjectTo {
      cp.add(GCC)
    } exploration {
      cp.binaryFirstFail(GCC.getX)
      println((GCC.getX map (_.getValue())).mkString(";"))
      GCC.check() should be (true)
      nbSol += 1
    }
    // Compare with brute force approach
    
  }


}

object TestGCCFWC {
  def domainGen(yMax: Int) = for {
    lb <- Gen.choose(0, yMax)
    ub <- Gen.choose(lb, yMax)
  } yield (lb, ub)
  
  /**
   * Generate an array X of m integer variables with random domains in [0,yMax]
   * Generate a minValue between 0 and MaxMinVal
   * Generate a number n of cardinality constraints (between 1 and yMax - minValue)
   * Generate n lower bounds between 0 and LB
   * Generate n upper bounds between lb and UB
   */
  def GCCGen(cp: CPSolver): GCCFWC = {
    val m = 10
    val yMax = 10
    val LB = 2
    val UB = 4
    val MaxMinVal = 5
    
    val X = Array.tabulate(m)(v => {
      val (lb,ub) = domainGen(yMax).sample.get
      new CPVarInt(cp, lb, ub)}
    )
    val minVal = Gen.choose(0, MaxMinVal).sample.get
    val n = Gen.choose(1, yMax-minVal).sample.get
    val Low = Array.tabulate(n)(v => Gen.choose(0,LB).sample.get)
    val Up = Low map (Gen.choose(_,UB).sample.get)
    
    new GCCFWC(X, minVal, Low, Up)
  }
}
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
      cp.post(new GCCFWC(x, minVal, Low, Up))
    } exploration {
      cp.binaryFirstFail(x)
      nbSol += 1
    }

    nbSol should be(0)
  }
//
//  test("Test 2: too many variables to satisfy the upper bounds of CC") {
//    val cp = new CPSolver()
//    var x1 = new CPVarInt(cp, 0, 2)
//    var x2 = new CPVarInt(cp, 1, 3)
//    var x3 = new CPVarInt(cp, 0, 3)
//    val x = Array(x1, x2, x3)
//
//    val minVal = 0
//    val Low = Array(0, 0, 0, 0)
//    val Up = Array(2, 0, 0, 0)
//
//    var nbSol = 0;
//    cp.solveAll subjectTo {
//      cp.add(new GCCFWC(x, minVal, Low, Up))
//    } exploration {
//      cp.binaryFirstFail(x)
//      nbSol += 1
//    }
//
//    nbSol should be(0)
//  }

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

    nbSol should be(2)
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

    nbSol should be(14)

  }

  test("Test 7: Checking random instances and comparing to an alternative approach") {
    println("Test 7")
    var nbSol = 0
    var nbSolAlternative = 0


    // Generate the constraint
    val cp = new CPSolver()
    val GCC = GCCGen(cp)
    val cp2 = new CPSolver()
    val o: Array[CPVarInt] = Array.tabulate(GCC.low.size) ( v => {
     new CPVarInt(cp2, GCC.low(v), GCC.up(v)) 
    })
    val X2 = GCC.X map (x => new CPVarInt(cp2,x.getMin(), x.getMax()) )
    println(GCC.X.mkString(";"))
    println(X2.mkString(";"))
    println(o.mkString(";"))
    
    // Solve with GCCFWC
    cp.solveAll subjectTo {
      cp.post(GCC)
    } exploration {
      cp.binaryFirstFail(GCC.X)
      //println((GCC.X map (_.getValue())).mkString(";"))
      GCC.check() should be(true)
      nbSol += 1
    }

    // Compare with GCCVar
    cp2.solveAll subjectTo {
      cp2.post(new GCCVar(X2,GCC.minVal,o))
    } exploration {
      cp2.binaryFirstFail(X2)
      //println((X2 map (_.getValue())).mkString(";"))
      nbSolAlternative += 1
    }

    // Compare with brute force approach
    nbSol should be (nbSolAlternative)
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
    val m = 5
    val yMax = 5
    val LB = 0
    val UB = 2
    val MaxMinVal = 3

    val X = Array.tabulate(m)(v => {
      val (lb, ub) = domainGen(yMax).sample.get
      new CPVarInt(cp, lb, ub)
    })
    val minVal = Gen.choose(0, MaxMinVal).sample.get
    val n = Gen.choose(1, yMax - minVal).sample.get
    val Low = Array.tabulate(n)(v => Gen.choose(0, LB).sample.get)
    val Up = Low map (Gen.choose(_, UB).sample.get)

    new GCCFWC(X, minVal, Low, Up)
  }
}
/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.modeling._
import oscar.cp.search.BinaryFirstFailBranching
import oscar.cp.scheduling.Activity

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class TestUnaryResource extends FunSuite with ShouldMatchers {

  // decomp without resource variables
  def decomp(cp: CPSolver, starts: Array[CPVarInt], durations: Array[CPVarInt], ends: Array[CPVarInt]): Unit = {
    val n = starts.size
    for (i <- 0 until n; j <- i + 1 until n) {
      cp.add((ends(i) <== starts(j)) || (ends(j) <== starts(i)))
    }
  }
  
  // decomp with resource variables
  def decomp(cp: CPSolver, starts: Array[CPVarInt], durations: Array[CPVarInt], ends: Array[CPVarInt],resources: Array[CPVarInt], id: Int ): Unit = {
    val n = starts.size
    for (i <- 0 until n; j <- i + 1 until n) {
      cp.add((ends(i) <== starts(j)) || (ends(j) <== starts(i)) || (resources(i) !== id) || (resources(j) !== id))
    }
  }  

  def unary(cp: CPSolver, starts: Array[CPVarInt], durations: Array[CPVarInt], ends: Array[CPVarInt]): Unit = {
    cp.add(unaryResource(starts, durations, ends))
  }

  def randomDurations(n: Int, seed: Int = 0): Array[Int] = {
    val rand = new scala.util.Random(seed)
    Array.tabulate(n)(i => 1 + rand.nextInt(15))
  }
  
  def randomInstance(n: Int, seed: Int = 0) = {
    val rand = new scala.util.Random(seed)
    Array.tabulate(n)(i => (1 + rand.nextInt(3),rand.nextBoolean))
  }
  
  test("unary unit 1") {

      
      implicit val cp = CPSolver()
      val starts = Array(CPVarInt(10), CPVarInt(7), CPVarInt(10))
      val durations = Array(3,1,3)
      val durs = durations.map(d => CPVarInt(d))
      val ends = Array.tabulate(starts.size)(i => starts(i) + durations(i))
      val required = Array(CPVarBool(false),CPVarBool(true),CPVarBool(false))
      add(unaryResource(starts,durs,ends, required))
      cp.isFailed should be(false)
  }  
  
  test("unary vs cumul") {
    for (i <- 0 to 10) {
      val n = 4
      val inst = randomInstance(5,i)
      
      val durations = inst.map(_._1)
      val optional = inst.map(_._2)
      val horizon = durations.sum
      val cp = CPSolver()
      val starts = Array.tabulate(n)(i => CPVarInt(cp, 0 until (horizon - durations(i) + 1)))
      val durs = Array.tabulate(n)(i => CPVarInt(cp, durations(i)))
      val ends = Array.tabulate(n)(i => starts(i) + durations(i))
      val resources = Array.tabulate(n)(i => if (optional(i)) CPVarInt(0 to 1)(cp) else CPVarInt(0)(cp) )
      cp.search {
        binaryFirstFail(starts) ++ binaryFirstFail(resources)
      }
      
      val statCum = cp.startSubjectTo() {
        cp.add(new SweepMaxCumulative(starts, ends, durs,starts.map(_ => CPVarInt(1)(cp)), resources, CPVarInt(1)(cp),0))
      }
      val statUnary = cp.startSubjectTo() {
        cp.add(unaryResource(starts,durs,ends, resources,0))
      }
      
      val statUnaryDecomp = cp.startSubjectTo() {
        decomp(cp,starts,durs,ends, resources,0)
      }
      
      statCum.nbSols should be (statUnary.nbSols)
      statUnaryDecomp.nbSols should be (statUnary.nbSols)
    }

     
  }
  

  
 
  test("decomp vs global, permutations") {

    def testPermutations(seed: Int) {
      def factorial(n: Int): Int = if (n == 0) 1 else n * factorial(n - 1)

      val n = 5
      val durations = randomDurations(n, seed)
      val horizon = durations.sum
      val cp = CPScheduler(horizon)
      val starts = Array.tabulate(n)(i => CPVarInt(cp, 0 until (horizon - durations(i) + 1)))
      val durs = Array.tabulate(n)(i => CPVarInt(cp, durations(i)))
      val ends = Array.tabulate(n)(i => starts(i) + durations(i))

      cp.onSolution {
        val p = (0 until n).sortBy(i => starts(i).value)
        //println("solution:" + p.map(i => "[" + starts(i).value + "," + ends(i).value + "]"))
      }

      cp.search {
        binaryFirstFail(starts)
      }

      val statDecomp = cp.startSubjectTo() {
        decomp(cp, starts, durs, ends)
      }
      
      val statGlobal = cp.startSubjectTo() {
        unary(cp, starts, durs, ends)
      }
      
      statDecomp.nbSols should be(statGlobal.nbSols)
      statDecomp.nbSols should be(factorial(n))
    }
    for (i <- 0 until 10) {
      testPermutations(i)
    }

  }
  
/*
	test("Test 1: packing") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2)
		val acts = Array(act1, act2, act3, act4)
		
		val resource1 = UnitResource(cp)
		val resource2 = UnitResource(cp)
		
		act1.needs(resource1)
		act2.needs(resource2)
		act3.needs(resource2)
		act4.needs(resource1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
		
		cp.solve subjectTo {
			
			act1 endsBeforeStartOf act2
			act3 endsBeforeStartOf act4
			
		} exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		} run()
		
		nSol should be(4)
	}
	
	test("Test 2: durations") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3 to 4)
		val act2 = Activity(cp, 1)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 2)
		val acts = Array(act1, act2, act3, act4)
		
		val resource1 = UnitResource(cp)
		val resource2 = UnitResource(cp)
		
		act1.needs(resource1)
		act2.needs(resource2)
		act3.needs(resource2)
		act4.needs(resource1)
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 3), (0, 4, 0, 3), (0, 3, 1, 3), (0, 4, 1, 3))
		
		cp.solve subjectTo {
			
			act1 precedes act2
			act3 precedes act4
			
		} exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		} run()
		
		act1.dur.value should be(3)
		nSol should be(4)
	}
	
	test("Test 3: durations") {	
		
		val horizon = 5
		val cp = new CPScheduler(horizon)		

		val act1 = Activity(cp, 3 to 4) //Should be reduced to 3 
		val act2 = Activity(cp, 2)
		val act3 = Activity(cp, 2)
		val act4 = Activity(cp, 1)
		val acts = Array(act1, act2, act3, act4)
		
		val resource1 = UnitResource(cp)
		val resource2 = UnitResource(cp)
		
		act1 needs resource1
		act2 needs resource1
		act3 needs resource2
		act4 needs resource2
		
		var nSol = 0
		
		val expectedSol = Set((0, 3, 0, 2),
							  (0, 3, 0, 3),
							  (0, 3, 0, 4), 
							  (0, 3, 1, 0), 
							  (0, 3, 1, 3), 
							  (0, 3, 1, 4), 
							  (0, 3, 2, 0),
							  (0, 3, 2, 1), 
							  (0, 3, 2, 4),
							  (0, 3, 3, 0), 
							  (0, 3, 3, 1), 
							  (0, 3, 3, 2), 
							  (2, 0, 0, 2),
							  (2, 0, 0, 3),
							  (2, 0, 0, 4), 
							  (2, 0, 1, 0), 
							  (2, 0, 1, 3), 
							  (2, 0, 1, 4), 
							  (2, 0, 2, 0),
							  (2, 0, 2, 1), 
							  (2, 0, 2, 4),
							  (2, 0, 3, 0), 
							  (2, 0, 3, 1), 
							  (2, 0, 3, 2))
		
		cp.addResourceConstraints()
		cp.solve 
		cp.exploration {
			cp.binary(acts.map(_.start))
			
			val sol = (act1.est, act2.est, act3.est, act4.est)
			expectedSol.contains(sol) should be(true)
			nSol += 1
		} run()
		
		act1.dur.value should be(3)
		nSol should be(24)
	} 
 */
	


}

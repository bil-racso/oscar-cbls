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

import oscar.cp._
import oscar.cp.testUtils._
import oscar.util.RandomGenerator

/**
 * Created on 06/02/15.
 * @author Cyrille Dejemeppe (cyrille.dejemeppe@gmail.com)
 */
class TestSetTimesBranching extends TestSuite {

  def splitRectangle(leftBound: Int, rightBound: Int, minWidth: Int, remainingSplits: Int): List[(Int, Int)] = {
    if (remainingSplits == 0 || (rightBound - leftBound) < 2 * minWidth) {
      List((leftBound, rightBound))
    }
    else {
      val minR = leftBound + minWidth
      val randomSplit = minR + RandomGenerator.nextInt(rightBound - minR)
      splitRectangle(leftBound, randomSplit, minWidth, remainingSplits - 1) ::: splitRectangle(randomSplit, rightBound, minWidth, remainingSplits - 1)
    }
  }

  test("SetTimes test on a dense rectangle of height 4 and width 100") {
    val minWidth = 10
    val optimalMakespan = 100
    val capacity = 4
    val maxRecursiveSplits = 5

    for (i <- 1 to 10) {
      val activitySolution = Array.tabulate(capacity)(i => splitRectangle(0, optimalMakespan, minWidth, maxRecursiveSplits)).flatten
      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._2 - a._1)

      val cp = CPSolver()
      cp.silent = true
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to optimalMakespan - durations(i))(cp))
      val endVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i) to optimalMakespan)(cp))
      val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(cp))
      val demandVars = Array.fill(nActivities)(CPIntVar(1)(cp))
      val makespan = maximum(endVars)

      for (i <- 0 until nActivities) {
        cp.add(startVars(i) + durationVars(i) == endVars(i))
      }

      cp.add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)(cp)))

      cp.minimize(makespan)
      cp.search{
        setTimes(startVars, durationVars, endVars,i => -endVars(i).min)
      }

      var bestSol = 0
      cp.onSolution{
        bestSol = makespan.value
      }

      cp.start()

      bestSol shouldEqual optimalMakespan
    }
  }

  test("SetTimes test on a dense rectangle of height 10 and width 1000") {
    val minWidth = 10
    val optimalMakespan = 1000
    val capacity = 10
    val maxRecursiveSplits = 10

    for (i <- 1 to 10) {
      val activitySolution = Array.tabulate(capacity)(i => splitRectangle(0, optimalMakespan, minWidth, maxRecursiveSplits)).flatten
      val nActivities = activitySolution.length
      val durations = activitySolution.map(a => a._2 - a._1)

      val cp = CPSolver()
      cp.silent = true
      val startVars = Array.tabulate(nActivities)(i => CPIntVar(0 to optimalMakespan - durations(i))(cp))
      val endVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i) to optimalMakespan)(cp))
      val durationVars = Array.tabulate(nActivities)(i => CPIntVar(durations(i))(cp))
      val demandVars = Array.fill(nActivities)(CPIntVar(1)(cp))
      val makespan = maximum(endVars)

      for (i <- 0 until nActivities) {
        cp.add(startVars(i) + durationVars(i) == endVars(i))
      }

      cp.add(maxCumulativeResource(startVars, durationVars, endVars, demandVars, CPIntVar(capacity)(cp)))

      cp.minimize(makespan)
      cp.search{
        setTimes(startVars, durationVars, endVars,i => -endVars(i).min)
      }

      var bestSol = 0
      cp.onSolution{
        bestSol = makespan.value
      }

      cp.start()

      bestSol shouldEqual optimalMakespan
    }
  }

  test("SetTimes with transitions and precedences") {
    // (duration, consumption)
    
    


    def solve(seed: Int, withSetTimes: Boolean): Int = {
      
      val rand = new scala.util.Random(seed)
      val nTasks = 7
      val instance = Array.tabulate(nTasks)(t => (rand.nextInt(30)+1,rand.nextInt(4)+1))
      val durationsData = instance.map(_._1)
      val demandsData = instance.map(_._2)
      val capa = 5
      val horizon = instance.map(_._1).sum
      val Times = 0 to horizon
      
      
      
      implicit val cp = CPSolver()
      cp.silent = true

      val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
      val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t).min))
      val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
      val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
      val makespan = maximum(ends)

      try {
        add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(capa)), Weak)

        val possPrec = for (i <- 0 until nTasks; j <- i + 1 until nTasks) yield (i, j)

        val prec = for (i <- 0 until 20) yield {
          val (a, b) = possPrec(rand.nextInt(possPrec.size))
          (a, b, rand.nextInt(10))
        }

        
        add(starts(0) >= 5)
        add(ends(0) <=  ends(0).min +4)
        
        for ((i, j, d) <- prec) {
          add(ends(i)+d <= starts(j))
          //add(ends(i) <= starts(j)+d)
          //add(starts(i) + d <= starts(j)) // settimes failw with this
        }
      } catch {
        case e: NoSolutionException => return 0
      }

      minimize(makespan)

      var best = Int.MaxValue

      onSolution {
        best = makespan.min
      }

      search {
        if (withSetTimes) setTimes(starts, durations, ends)
        else binaryStatic(starts,_.min)

      }

      val stats = start()
      //println(stats)
      //println("obj:"+best)
      best

    }
    for (i <- 0 until 10000) {
      val opt1 = solve(i, true)
      val opt2 = solve(i, false)
      assert(opt1 == opt2)
    }


  }
  
  
  test("SetTimes Test Dominance") {
    // (duration, consumption)
    
    


    def solve(seed: Int, withSetTimes: Boolean): Int = {
      
      val rand = new scala.util.Random(seed)
      val nTasks = 4
      val instance = Array.tabulate(nTasks)(t => (5,1))
      val durationsData = instance.map(_._1)
      val demandsData = instance.map(_._2)
      val capa = 1
      val horizon = 100
      val Times = 0 to horizon
      
      
      
      implicit val cp = CPSolver()
      cp.silent = true

      val durations = Array.tabulate(nTasks)(t => CPIntVar(durationsData(t)))
      val starts = Array.tabulate(nTasks)(t => CPIntVar(0, horizon - durations(t).min))
      val ends = Array.tabulate(nTasks)(t => starts(t) + durationsData(t))
      val demands = Array.tabulate(nTasks)(t => CPIntVar(demandsData(t)))
      val makespan = maximum(ends)

      try {
        add(maxCumulativeResource(starts, durations, ends, demands, CPIntVar(capa)), Weak)


        add(ends(0) <=  10)
        for (i <- 1 until nTasks) {
          add(starts(i) >= 8)
        }

      } catch {
        case e: NoSolutionException => return 0
      }

      minimize(makespan)

      var best = Int.MaxValue

      onSolution {
        best = makespan.min
      }

      search {
        if (withSetTimes) setTimes(starts, durations, ends)
        else binaryStatic(starts,_.min)

      }

      val stats = start()
      //println(stats)
      //println("obj:"+best)
      best

    }
    for (i <- 0 until 10000) {
      val opt1 = solve(i, true)
      val opt2 = solve(i, false)
      assert(opt1 == opt2)
    }


  }  
  
    

}

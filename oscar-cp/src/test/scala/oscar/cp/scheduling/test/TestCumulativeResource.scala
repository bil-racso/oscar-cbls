/**
 * *****************************************************************************
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
 * ****************************************************************************
 */
package oscar.cp.scheduling.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._

import oscar.cp.modeling._
import oscar.cp.scheduling._
import oscar.algo.reversible._

import oscar.visual._

class TestCumulativeResource extends FunSuite with ShouldMatchers {

  test("Test 1: packing") {

    val horizon = 5
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 4)
    val act2 = Activity(cp, 3)
    val act3 = Activity(cp, 2)
    val acts = Array(act1, act2, act3)

    val resource = MaxResource(cp, 2)

    act1 needs 1 ofResource resource
    act2 needs 1 ofResource resource
    act3 needs 1 ofResource resource

    var nSol = 0

    val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))

    cp.addResourceConstraints()
    cp.solve
    cp.search {
      binaryStatic(acts.map(_.start))
    } onSolution {
      val sol = (act1.est, act2.est, act3.est)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    }
    cp.start()

    nSol should be(4)
  }

  test("Test 2: durations") {

    val horizon = 5
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 3)
    val act2 = Activity(cp, 1 to 2)
    val act3 = Activity(cp, 1 to 4)
    val acts = Array(act1, act2, act3)

    val resource = MaxResource(cp, 1)

    act1 needs 1 ofResource resource
    act2 needs 1 ofResource resource
    act3 needs 1 ofResource resource

    var nSol = 0

    val expectedSol = Set((0, 3, 4), (0, 4, 3), (1, 0, 4), (1, 4, 0), (2, 0, 1), (2, 1, 0))

    cp.addResourceConstraints()
    cp.solve()
    cp.search {
      binaryStatic(acts.map(_.start))
    } onSolution {
      val sol = (act1.est, act2.est, act3.est)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()

    act2.dur.value should be(1)
    act3.dur.value should be(1)
    nSol should be(6)
  }

  test("Test 3: height") {

    val horizon = 5
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 4)
    val act2 = Activity(cp, 3)
    val act3 = Activity(cp, 2)
    val acts = Array(act1, act2, act3)

    val resource = MaxResource(cp, 2)

    act1 needs (1 to 2) ofResource resource
    act2 needs 1 ofResource resource
    act3 needs 1 ofResource resource

    var nSol = 0

    val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))

    cp.addResourceConstraints()
    cp.solve()
    cp.search {
      binaryStatic(acts.map(_.start))
    } onSolution {
      val sol = (act1.est, act2.est, act3.est)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()

    resource.heightOf(act1).value should be(1)

    nSol should be(4)
  }

  test("Test 4: producer + durations") {

    val horizon = 4
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 2 to 3)
    val act2 = Activity(cp, 1)
    val act3 = Activity(cp, 2)
    val act4 = Activity(cp, 2 to 3)
    val acts = Array(act1, act2, act3, act4)

    val resource = MaxResource(cp, 2)

    act1 needs 3 ofResource resource
    act2 needs 2 ofResource resource
    act3 needs 1 ofResource resource
    act4 gives 1 toResource resource

    var nSol = 0

    val expectedSol = Set((0, 2, 2, 0),
      (2, 1, 0, 1))

    cp.addResourceConstraints()
    cp.solve()
    cp.search {
      binaryStatic(acts.map(_.start))
    } onSolution {
      act1.dur.value should be(2)
      act4.dur.value should be(3)
      val sol = (act1.est, act2.est, act3.est, act4.est)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()


    nSol should be(2)
  }

  test("Test 5: producer + durations + height") {

    val horizon = 4
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 2 to 3)
    val act2 = Activity(cp, 1)
    val act3 = Activity(cp, 2)
    val act4 = Activity(cp, 2 to 3)

    val resource = MaxResource(cp, 2)

    act1 needs 4 ofResource resource
    act2 needs 3 ofResource resource
    act3 needs 1 ofResource resource
    act4 gives (1 to 2) toResource resource

    var nSol = 0

    val expectedSol = Set((0, 2, 2, 0),
      (2, 1, 0, 1))

    cp.addResourceConstraints()
    cp.solve
    cp.search {
      binaryStatic(cp.activities.map(_.start))
    } onSolution {
      act1.dur.value should be(2)
      act4.dur.value should be(3)
      val sol = (act1.est, act2.est, act3.est, act4.est)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()


    resource.heightOf(act4).value should be(-2)
    nSol should be(2)
  }

  test("Test 6: min resource") {

    val horizon = 6
    val cp = CPScheduler(horizon)

    val act1 = Activity(cp, 6)
    val act2 = Activity(cp, 2)
    val act3 = Activity(cp, 4)

    val resource = MinResource(cp, 1)

    act1 gives 1 toResource resource
    act2 needs 2 ofResource resource
    act3 needs 2 ofResource resource

    var nSol = 0

    val expectedSol = Set((0, 0, 2),
      (0, 4, 0))

    cp.solve subjectTo {

    } search {
      binaryStatic(cp.activities.map(_.start))
    } onSolution {
      val sol = (act1.est, act2.est, act3.est)
      println(act1.start + " " + act2.start + " " + act3.start)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()

    nSol should be(2)
  }

  test("Test 7: unsolvable problem") {
    val cp = CPScheduler(100)
    val act1 = Activity(cp, 30)
    val act2 = Activity(cp, 30)
    val resource = MaxResource(cp, 5)

    act2 gives 5 toResource resource
    act1 needs 6 ofResource resource // act1 needs more than act2 provides

    try {
      cp.solve subjectTo {
        act1.endsBeforeStartOf(act2) // act2 can't provide resource until after act1
      } search {
        binaryStatic(cp.activities.map(_.start))
      } start()
    } catch {
      case e: NoSolutionException => fail("Caught NoSolutionException. This should be handled internally")
    }

    cp.isFailed() should be(true)
  }
  
  test("Test 1: MinSweepCumulative") {

    val horizon = 8
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 2 to 4)
    val act2 = Activity(cp, 0 to 2)

    val cumAct1 = CumulativeActivity(act1, 0, -1 to 1)
    val cumAct2 = CumulativeActivity(act2, 0 to 1, -3 to 4)

    cp.add(act1.start >= 1)
    cp.add(act1.start <= 2)

    cp.add(act2.start >= 0)
    cp.add(act2.start <= 6)

    val acts = Array(cumAct1, cumAct2)

    val constraint1 = SweepMinCumulative(cp, acts, CPVarInt(cp, 4), 0)
    val constraint2 = SweepMinCumulative(cp, acts, CPVarInt(cp, 3), 1)

    cp.add(constraint1)
    cp.add(constraint2)

    cumAct2.est should be(1)
    cumAct2.lst should be(2)
    cumAct2.ect should be(3)
    cumAct2.lct should be(4)
    cumAct2.minDuration should be(1)
    cumAct2.maxDuration should be(2)
    cumAct2.minHeight should be(3)
    cumAct2.maxHeight should be(4)
  }

  test("Test 2: alternatives") {

    val horizon = 6
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 4)
    val act3 = CumulativeActivity(Activity(cp, 6), 0 to 1, 2)
    val act4 = CumulativeActivity(Activity(cp, 6), 0 to 1, 3)
    val acts = Array(act1, act2, act3, act4)

    var nbSol = 0
    val expectedSol = Set((0, 0, 1, 1), (1, 1, 0, 0))

    cp.solve subjectTo {

      cp.add(SweepMaxCumulative(cp, acts, CPVarInt(cp, 5), 0))
      cp.add(SweepMaxCumulative(cp, acts, CPVarInt(cp, 5), 1))
      cp.add(SweepMinCumulative(cp, acts, CPVarInt(cp, 5), 0))
      cp.add(SweepMinCumulative(cp, acts, CPVarInt(cp, 5), 1))

    } search {
      binaryStatic(acts.map(_.resource))
    } onSolution {
      val sol = (acts(0).resource.value, acts(1).resource.value, acts(2).resource.value, acts(3).resource.value)
      expectedSol.contains(sol) should be(true)
      nbSol += 1
    } start()
    nbSol should be(2)
  }

  test("Test 3: alternatives") {

    val horizon = 6
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 4)
    val act3 = CumulativeActivity(Activity(cp, 6), 0 to 1, 2)
    val act4 = CumulativeActivity(Activity(cp, 6), 0 to 1, 3)
    val acts = Array(act1, act2, act3, act4)

    var nbSol = 0
    val expectedSol = Set((0, 0, 1, 1), (1, 1, 0, 0))

    cp.solve subjectTo {

      cp.add(SweepMaxCumulative(cp, acts, CPVarInt(cp, 5), 0))
      cp.add(SweepMaxCumulative(cp, acts, CPVarInt(cp, 5), 1))
      cp.add(acts(1).resource == 0)

      acts(0).resource.value should be(0)
      acts(2).resource.value should be(1)
      acts(3).resource.value should be(1)

    } search {
      binaryStatic(acts.map(_.resource))
    }
    cp.start().nbSols should be(1)
  }

  test("Test 4: MaxSweepCumulative") {

    val horizon = 106
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 4)
    val act3 = CumulativeActivity(Activity(cp, 6), 0 to 1, 2)
    val act4 = CumulativeActivity(Activity(cp, 6), 0 to 1, 3)
    val act5 = CumulativeActivity(Activity(cp, 6), 0 to 1, 1)
    val acts = Array(act1, act2, act3, act4, act5)

    cp.add(act1.start == 0)
    cp.add(act2.start == 0)
    cp.add(act3.start == 0)
    cp.add(act4.start == 0)

    cp.add(SweepMaxCumulative(cp, acts, CPVarInt(cp, 5), 0))
    cp.add(SweepMaxCumulative(cp, acts, CPVarInt(cp, 5), 1))
    cp.add(acts(1).resource == 0)

    acts(0).resource.value should be(0)
    acts(2).resource.value should be(1)
    acts(3).resource.value should be(1)
    acts(4).resource.size should be(2)
    cp.add(acts(4).resource == 0)
    acts(4).start.min should be(6)

  }

  test("Test 5: MaxSweepCumulative") {

    val horizon = 6
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 6), 0 to 1, 0 to 5)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, -2 to 5)

    cp.add(SweepMaxCumulative(cp, Array(act1, act2), CPVarInt(cp, 3), 0))

    cp.add(act1.resource == 0)
    cp.add(act2.resource == 0)
    cp.add(act1.height == 5) // it means act2 must have a negative consumption of -2

    act2.height.value should be(-2)

  }

  test("Test 6: MaxSweepCumulative") {

    val horizon = 9
    val cp = new CPScheduler(horizon)

    val act1 = CumulativeActivity(Activity(cp, 0 to 5), 0 to 1, 0 to 5)
    val act2 = CumulativeActivity(Activity(cp, 6), 0 to 1, 0 to 5)

    cp.add(SweepMaxCumulative(cp, Array(act1, act2), CPVarInt(cp, 3), 0))

    cp.add(act1.resource == 0)
    cp.add(act1.height == 2)
    cp.add(act2.height == 2)
    cp.add(act2.resource == 0)
    cp.add(act2.start == 3)

    act1.dur.min should be(0)
    act1.dur.max should be(3)

  }
  
  test("RCPSP1") {

    // (duration, consumption)
    val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2))
    val capa = 4
    val horizon = instance.map(_._1).sum
    val Times = 0 to horizon

    val cp = CPScheduler(horizon)

    var bestObj = horizon
    val tasks: Array[CumulativeActivity] = instance.map { case (dur, req) => CumulativeActivity(cp, dur, 0, req) }
    val makespan = maximum(tasks.map(_.end))
    cp.minimize(makespan) subjectTo {
      cp.add(SweepMaxCumulative(cp, tasks, CPVarInt(cp, capa), 0))
    } search {
      setTimes(tasks.map(_.start),tasks.map(_.dur),tasks.map(_.end))
    } onSolution {
      bestObj = makespan.value
    } start()
    bestObj should be(160)

  }

  test("RCPSP2") {
	println("test2")
    // (duration, consumption)
    val instance = Array((50, 1), (30, 1), (90, 3), (10, 2), (20, 2), (80, 1), (30, 2), (20, 2), (20, 1), (10, 1), (10, 2))
    val capa = 4
    val horizon = instance.map(_._1).sum
    val Times = 0 to horizon

    val cp = CPScheduler(horizon)

    var bestObj = horizon

    val resource = MaxResource(cp, capa)
    val activities = instance.map(a => Activity(cp, a._1))

    val makespan = cp.makespan

    cp.minimize(makespan) subjectTo {
		for (a <- 0 until instance.size)
			activities(a) needs instance(a)._2 ofResource resource
    } search {
      setTimes(activities.map(_.start),activities.map(_.dur),activities.map(_.end))
    } onSolution {
      bestObj = makespan.value
    } start()
    println("=>"+bestObj)
    cp.printStats()
    bestObj should be(160)

  }
}

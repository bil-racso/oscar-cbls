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
import scala.Array.canBuildFrom

class TestAlternativeCumulativeResource extends FunSuite with ShouldMatchers {

  test("Test 1: packing") {

    val horizon = 5
    val nTasks = 3
    val Tasks = 0 until nTasks
    val nResources = 2
    val Resources = 0 until nResources
    val durationsData = Array(4, 3, 2)
    val demandsData = Array(1, 1, 1)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1)
    val capa = Array(2, 2)

    val cp = new CPSolver()
    val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, demandsData(t)))
    val resources = Array.tabulate(nTasks)(t => CPVarInt(cp, resourcesData(t)))

    var nSol = 0
    val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))

    cp.solve subjectTo {
      // Consistency 
      for (t <- Tasks) {
        cp.add(ends(t) == starts(t) + durations(t))
      }
      // Cumulative
      for (r <- Resources) {
        cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
      }

      cp.add(resources(0) == 0)
      cp.add(resources(1) == 0)
      cp.add(resources(2) == 0)

    } exploration {
      cp.binary(starts)
      val sol = (starts(0).min, starts(1).min, starts(2).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } run ()

    nSol should be(4)
  }

  test("Test 2: durations") {

    val horizon = 5
    val nTasks = 3
    val Tasks = 0 until nTasks
    val nResources = 2
    val Resources = 0 until nResources
    val durationsData = Array(3 to 3, 1 to 2, 1 to 4)
    val demandsData = Array(1, 1, 1)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1)
    val capa = Array(1, 1)

    val cp = new CPSolver()
    val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, demandsData(t)))
    val resources = Array.tabulate(nTasks)(t => CPVarInt(cp, resourcesData(t)))

    var nSol = 0
    val expectedSol = Set((0, 3, 4), (0, 4, 3), (1, 0, 4), (1, 4, 0), (2, 0, 1), (2, 1, 0))

    cp.solve subjectTo {
      // Consistency 
      for (t <- Tasks) {
        cp.add(ends(t) == starts(t) + durations(t))
      }
      // Cumulative
      for (r <- Resources) {
        cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
      }

      cp.add(resources(0) == 0)
      cp.add(resources(1) == 0)
      cp.add(resources(2) == 0)

    } exploration {
      cp.binary(starts)
      val sol = (starts(0).min, starts(1).min, starts(2).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } run ()

    durations(1).value should be(1)
    durations(2).value should be(1)
    nSol should be(6)
  }

  test("Test 3: height") {

    val horizon = 5
    val durationsData = Array(4, 3, 2)
    val demandsData = Array(1 to 2, 1 to 1, 1 to 1)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1)
    val capa = Array(2, 2)

    val nTasks = demandsData.size
    val Tasks = 0 until nTasks
    val nResources = capa.size
    val Resources = 0 until nResources

    val cp = new CPSolver()
    val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, demandsData(t)))
    val resources = Array.tabulate(nTasks)(t => CPVarInt(cp, resourcesData(t)))

    var nSol = 0
    val expectedSol = Set((0, 0, 3), (1, 0, 3), (0, 2, 0), (1, 2, 0))

    cp.solve subjectTo {
      // Consistency 
      for (t <- Tasks) {
        cp.add(ends(t) == starts(t) + durations(t))
      }
      // Cumulative
      for (r <- Resources) {
        cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
      }

      cp.add(resources(0) == 0)
      cp.add(resources(1) == 0)
      cp.add(resources(2) == 0)

    } exploration {
      cp.binary(starts)
      val sol = (starts(0).min, starts(1).min, starts(2).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } run ()

    demands(0).value should be(1)
    nSol should be(4)
  }

  test("Test 4: alternatives") {

    val horizon = 4
    val durationsData = Array(2 to 3, 1 to 1, 2 to 2, 2 to 3)
    val demandsData = Array(3, 2, 1, -1)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1, 0 to 1)
    val capa = Array(2, 2)

    val nTasks = demandsData.size
    val Tasks = 0 until nTasks
    val nResources = capa.size
    val Resources = 0 until nResources

    val cp = new CPSolver()
    val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, demandsData(t)))
    val resources = Array.tabulate(nTasks)(t => CPVarInt(cp, resourcesData(t)))

    var nSol = 0
    val expectedSol = Set((0, 2, 2, 0), (2, 1, 0, 1))

    cp.solve subjectTo {
      // Consistency 
      for (t <- Tasks) {
        cp.add(ends(t) == starts(t) + durations(t))
      }
      // Cumulative
      for (r <- Resources) {
        cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
      }

      cp.add(resources(0) == 0)
      cp.add(resources(1) == 0)
      cp.add(resources(2) == 0)
    }

    resources(3).value should be(0)

    cp.exploration {
      cp.binary(starts)
      val sol = (starts(0).min, starts(1).min, starts(2).min, starts(3).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } run ()

    durations(0).value should be(2)
    durations(3).value should be(3)
    nSol should be(2)
  }

  test("Test 5: alternatives") {

    val horizon = 4
    val durationsData = Array(2 to 3, 1 to 1, 2 to 2, 2 to 3)
    val demandsData = Array(4 to 4, 3 to 3, 1 to 1, -2 to -1)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1, 0 to 1)
    val capa = Array(2, 2)

    val nTasks = demandsData.size
    val Tasks = 0 until nTasks
    val nResources = capa.size
    val Resources = 0 until nResources

    val cp = new CPSolver()
    val durations = Array.tabulate(nTasks)(t => CPVarInt(cp, durationsData(t)))
    val starts = Array.tabulate(nTasks)(t => CPVarInt(cp, 0 to horizon - durations(t).min))
    val ends = Array.tabulate(nTasks)(t => CPVarInt(cp, durations(t).min to horizon))
    val demands = Array.tabulate(nTasks)(t => CPVarInt(cp, demandsData(t)))
    val resources = Array.tabulate(nTasks)(t => CPVarInt(cp, resourcesData(t)))

    var nSol = 0
    val expectedSol = Set((0, 2, 2, 0), (2, 1, 0, 1))

    cp.solve subjectTo {
      // Consistency 
      for (t <- Tasks) {
        cp.add(ends(t) == starts(t) + durations(t))
      }
      // Cumulative
      for (r <- Resources) {
        cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
      }

      cp.add(resources(0) == 0)
      cp.add(resources(1) == 0)
      cp.add(resources(2) == 0)
    }

    resources(3).value should be(0)

    cp.exploration {
      cp.binary(starts)
      val sol = (starts(0).min, starts(1).min, starts(2).min, starts(3).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } run ()

    durations(0).value should be(2)
    durations(3).value should be(3)
    demands(3).value should be(-2)
    nSol should be(2)
  }

  test("Test 6: alternatives") {

    val horizon = 6
    val cp = new CPScheduler(horizon)
    val req = Array(1, 4, 3, 2)

    val act1 = Activity(cp, 6)
    val act2 = Activity(cp, 6)
    val act3 = Activity(cp, 6)
    val act4 = Activity(cp, 6)
    val acts = Array(act1, act2, act3, act4)

    val resourceSet = AlternativeCumulativeResource(cp, 2, 5)

    act1 needs 1 ofResources (0 to 1) in resourceSet
    act2 needs 4 ofResources (0 to 1) in resourceSet
    act3 needs 3 ofResources (0 to 1) in resourceSet
    act4 needs 2 ofResources (0 to 1) in resourceSet

    var nbSol = 0

    cp.solve
    cp.subjectTo {

      cp.add(resourceSet.resourcesOf(act2) == 0)
    }

    resourceSet.resourcesOf(act2).value should be(0)
    resourceSet.resourcesOf(act3).value should be(1)
    resourceSet.resourcesOf(act4).value should be(1)

    cp.exploration {

      cp.binary(acts.map(resourceSet.resourcesOf(_)))
      nbSol += 1
    } run ()

    nbSol should be(1)
  }

  test("Test 7: alternatives") {

    val horizon = 106
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 6)
    val act2 = Activity(cp, 6)
    val act3 = Activity(cp, 6)
    val act4 = Activity(cp, 6)
    val act5 = Activity(cp, 6)
    val acts = Array(act1, act2, act3, act4, act5)

    val resource1 = MaxResource(cp, 5)
    val resource2 = MaxResource(cp, 5)

    val resourceSet = AlternativeCumulativeResource(resource1, resource2)

    act1 needs 1 ofResources (0 to 1) in resourceSet
    act2 needs 4 ofResources (0 to 1) in resourceSet
    act3 needs 3 ofResources (0 to 1) in resourceSet
    act4 needs 2 ofResources (0 to 1) in resourceSet
    act5 needs 1 ofResources (0 to 1) in resourceSet

    cp.add(act1.start == 0)
    cp.add(act2.start == 0)
    cp.add(act3.start == 0)
    cp.add(act4.start == 0)

    cp.add(resourceSet.resourcesOf(act2) == 0)

    cp.addResourceConstraints()

    resourceSet.resourcesOf(act1).value should be(0)
    resourceSet.resourcesOf(act3).value should be(1)
    resourceSet.resourcesOf(act4).value should be(1)

    resourceSet.resourcesOf(act5).size should be(2)

    cp.add(resourceSet.resourcesOf(act5) == 0)

    act5.start.min should be(6)
  }

  test("Test 8: alternatives") {

    val horizon = 11
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 0 to 5)
    val act2 = Activity(cp, 6)
    val acts = Array(act1, act2)

    val resourceSet = AlternativeCumulativeResource(cp, 2, 3)

    act1 needs (0 to 5) ofResources (0 to 1) in resourceSet
    act2 needs (0 to 5) ofResources (0 to 1) in resourceSet

    cp.add(act1.start == 0)

    cp.solve subjectTo {

      cp.add(resourceSet.resourcesOf(act1) == 0)
      cp.add(resourceSet.heightOf(act1) == 2)
      cp.add(resourceSet.heightOf(act2) == 2)
      cp.add(resourceSet.resourcesOf(act2) == 0)
      cp.add(act2.start == 3)
    }

    act1.dur.min should be(0)
    act1.dur.max should be(3)
  }

  test("Test 9: alternatives") {

    val horizon = 106
    val cp = new CPScheduler(horizon)

    val act1 = Activity(cp, 6)
    val act2 = Activity(cp, 6)
    val act3 = Activity(cp, 6)
    val act4 = Activity(cp, 6)
    val act5 = Activity(cp, 6)

    val resource1 = MaxResource(cp, 5)
    val resource2 = MaxResource(cp, 5)

    // Pool of the two previous resources
    val resourceSet = AlternativeCumulativeResource(resource1, resource2)

    // OR needs
    act1 needs 1 ofResources (0 to 1) in resourceSet
    act3 needs 3 ofResources (0 to 1) in resourceSet
    act4 needs 2 ofResources (0 to 1) in resourceSet
    act5 needs 1 ofResources (0 to 1) in resourceSet

    // AND needs
    act2 needs 4 ofResource resource1

    cp.add(act1.start == 0)
    cp.add(act2.start == 0)
    cp.add(act3.start == 0)
    cp.add(act4.start == 0)

    cp.addResourceConstraints()

    resourceSet.resourcesOf(act1).value should be(0)
    resourceSet.resourcesOf(act3).value should be(1)
    resourceSet.resourcesOf(act4).value should be(1)

    resourceSet.resourcesOf(act5).size should be(2)

    cp.add(resourceSet.resourcesOf(act5) == 0)

    act5.start.min should be(6)
  }
}

object TestCumulativeResourceSet extends App {

}

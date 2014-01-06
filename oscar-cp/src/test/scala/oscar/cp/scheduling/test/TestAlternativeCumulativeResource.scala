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

    } search {
      binaryStatic(starts)
    } onSolution { 
      val sol = (starts(0).min, starts(1).min, starts(2).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()

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

    } search {
      binaryStatic(starts)
    } onSolution {
      val sol = (starts(0).min, starts(1).min, starts(2).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()

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

    } search {
      binaryStatic(starts)
    } onSolution {
      val sol = (starts(0).min, starts(1).min, starts(2).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()

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


    
    cp.search {
      binaryStatic(starts)
    } onSolution {
      resources(3).value should be(0)
      durations(0).value should be(2)
      durations(3).value should be(3)
          
      val sol = (starts(0).min, starts(1).min, starts(2).min, starts(3).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()


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


    
    cp.search {
      binaryStatic(starts)
    } onSolution {
      durations(0).value should be(2)
      durations(3).value should be(3)
      demands(3).value should be(-2)
      val sol = (starts(0).min, starts(1).min, starts(2).min, starts(3).min)
      expectedSol.contains(sol) should be(true)
      nSol += 1
    } start()


    nSol should be(2)
  }

  test("Test 6: alternatives") {

    val horizon = 6
    val durationsData = Array(6, 6, 6, 6)
    val demandsData = Array(1, 4, 3, 2)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1, 0 to 1)
    val capa = Array(5, 5)

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

    cp.solve subjectTo {
      // Consistency 
      for (t <- Tasks) {
        cp.add(ends(t) == starts(t) + durations(t))
      }
      // Cumulative
      for (r <- Resources) {
        cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
      }

      cp.add(resources(1) == 0)
    }

    resources(0).value should be(0)
    resources(2).value should be(1)
    resources(3).value should be(1)

    cp.search {
      binaryStatic(resources)
    } onSolution {
      nSol += 1
    } start()

    nSol should be(1)
  }

  test("Test 7: alternatives") {

    val horizon = 106
    val durationsData = Array(6, 6, 6, 6, 6)
    val demandsData = Array(1, 4, 3, 2, 1)
    val resourcesData = Array(0 to 1, 0 to 1, 0 to 1, 0 to 1, 0 to 1)
    val capa = Array(5, 5)

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

    cp.add(starts(0) == 0)
    cp.add(starts(1) == 0)
    cp.add(starts(2) == 0)
    cp.add(starts(3) == 0)
    
    // Consistency 
    for (t <- Tasks) {
      cp.add(ends(t) == starts(t) + durations(t))
    }
    // Cumulative
    for (r <- Resources) {
      cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
    }

    cp.add(resources(1) == 0)

    resources(0).value should be(0)
    resources(2).value should be(1)
    resources(3).value should be(1)
    resources(4).size should be(2)

    cp.add(resources(4) == 0)

    starts(4).min should be(6)
  }

  test("Test 8: alternatives") {
    
    val horizon = 11
    val durationsData = Array(0 to 5, 6 to 6)
    val demandsData = Array(0 to 5, 0 to 5)
    val resourcesData = Array(0 to 1, 0 to 1)
    val capa = Array(3, 3)

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
    
    // Consistency 
    for (t <- Tasks) {
      cp.add(ends(t) == starts(t) + durations(t))
    }
    // Cumulative
    for (r <- Resources) {
      cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
    }

    cp.add(starts(0) == 0)
    cp.add(resources(0) == 0)
    cp.add(resources(1) == 0)
    cp.add(demands(0) == 2)
    cp.add(demands(1) == 2)
    cp.add(starts(1) == 3)
    
    durations(0).min should be(0)
    durations(0).max should be(3)
  }

  test("Test 9: alternatives") {
    
    val horizon = 106
    val durationsData = Array(6, 6, 6, 6, 6)
    val demandsData = Array(1, 4, 3, 2, 1)
    val resourcesData = Array(0 to 1, 0 to 0, 0 to 1, 0 to 1, 0 to 1)
    val capa = Array(5, 5)

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

    cp.add(starts(0) == 0)
    cp.add(starts(1) == 0)
    cp.add(starts(2) == 0)
    cp.add(starts(3) == 0)
    
    // Consistency 
    for (t <- Tasks) {
      cp.add(ends(t) == starts(t) + durations(t))
    }
    // Cumulative
    for (r <- Resources) {
      cp.add(new SweepMaxCumulative(starts, ends, durations, demands, resources, CPVarInt(cp, capa(r)), r))
    }

    cp.add(resources(1) == 0)

    resources(0).value should be(0)
    resources(2).value should be(1)
    resources(3).value should be(1)
    resources(4).size should be(2)

    cp.add(resources(4) == 0)

    starts(4).min should be(6)
  }
}

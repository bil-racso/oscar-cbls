/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *   
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *  
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *  
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/


package oscar.cp.mem.qap;

import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source
import java.lang._
import oscar.visual.VisualFrame
import oscar.cp.mem.visu.PlotPareto
import scala.collection.JavaConversions._
import oscar.visual.PlotLine
import oscar.cp.mem.measures.Hypervolume
import java.awt.Color


/**
 * Quadratic Assignment Problem:
 * There are a set of n facilities and a set of n locations.
 * For each pair of locations, a distance is specified and
 * for each pair of facilities a weight or flow is specified
 * (e.g., the amount of supplies transported between the two facilities).
 * The problem is to assign all facilities to different locations
 * with the goal of minimizing the sum of the distances multiplied by the corresponding flows.
 *
 * @author Pierre Schaus pschaus@gmail.com
 */
object QuadraticAssignmentMO3 extends App {
  
  
  /*
  val dataFile = "data/mo-qap/KC10-2fl-1uni.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-1uni.PO.txt"
  val n = 10  
  */  
  
  /*
  val dataFile = "data/mo-qap/KC10-2fl-2uni.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-2uni.PO.txt"
  val n = 10
  */
  
  /*
  val dataFile = "data/mo-qap/KC10-2fl-3uni.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-3uni.PO.txt"
  val n = 10
  */
  
  /*
  val dataFile = "data/mo-qap/KC10-2fl-1rl.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-1rl.PO.txt"
  val n = 10  
  */
  
  /*
  val dataFile = "data/mo-qap/KC10-2fl-2rl.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-2rl.PO.txt"
  val n = 10  
  */

  /*
  val dataFile = "data/mo-qap/KC10-2fl-3rl.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-3rl.PO.txt"
  val n = 10  
  */
  
  /*
  val dataFile = "data/mo-qap/KC10-2fl-4rl.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-4rl.PO.txt"
  val n = 10  
  */
  
  
  val dataFile = "data/mo-qap/KC10-2fl-5rl.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-5rl.PO.txt"
  val n = 10  
  
  
  /*
  val dataFile = "data/mo-qap/KC20-2fl-3uni.dat"
  val solFile = "data/mo-qap/solutions/KC10-2fl-5rl.PO.txt"
  val n = 20  
  */
  
  /*
  val dataFile = "data/mo-qap/qapUni.25.0.1"
  val solFile = "data/mo-qap/solutions/qapUni.25.0.1.500"
  val n = 25  
  */
  
  /*
  val dataFile = "data/mo-qap/qapUni.25.0.2"
  val solFile = "data/mo-qap/solutions/qapUni.25.0.2.500"
  val n = 25  
  */
  
  val N = 0 until n
  
  // Read the data
  val (d,w1,w2) = QAPReader.read(dataFile, n)
  // Read the solution
  val solutions = QAPReader.readSolutions(solFile, n)
  //val solutions = QAPReader.readSolutions2(solFile, n)
  
  
  val rand = new scala.util.Random(0)
  
  case class Sol(xsol: Array[Int])
  //val pareto: ListPareto[Sol] = ListPareto(2)
  //val paretoRef: ParetoSet[Sol] = ParetoSet(2)
  

  
  val xinit = rand.shuffle((0 until n).toList).toArray
  val obj1init =  (for(i <- N; j <- N) yield d(xinit(i))(xinit(j))*w1(i)(j)).sum
  val obj2init =  (for(i <- N; j <- N) yield d(xinit(i))(xinit(j))*w2(i)(j)).sum
  //pareto.insert(MOSol(Sol(xinit),Array(obj1init,obj2init)))
  
  //println((for(i <- N; j <- N) yield d(xs(i)-1)(xs(j)-1)*w2(i)(j)).sum)
  
  // Visualization
  val f = new VisualFrame("QAP",1,2)
  val plotPareto = new PlotPareto(nbPareto = 2)
  val plotHV = new PlotLine("Hype","progress","hype")
  f.createFrame("QAP Pareto Fron").add(plotPareto)
  f.createFrame("QAP HV").add(plotHV)
  f.pack()
  
  for ((xsol,o) <- solutions) {
    plotPareto.insert(o(0),o(1),1)
  }
  
  
  //val qapLNS = new QuadraticAssignmentWeightedSum(n,d,w1,w2)
  /*
  for (p <- 0 to 100 by 10) {
    println("solving with proba: "+p)
    val (xsol,o) = qapLNS.solve(p)
    val newSol = MOSol(Sol(xsol), o)
    pareto.insert(newSol)
  }*/
  
  /*
  for (obj <- 0 to 2 ) {
    println("solving with obj: "+obj)
    val (xsol: Array[Int],o: Array[Int]) = qapLNS.solveIt(obj)
    val newSol = MOSol(Sol(xsol), o)
    pareto.insert(newSol)
  }*/
  
  
  
  println("--------------")
  
  {
    var obj1Sol = Int.MaxValue
    var obj2Sol = Int.MaxValue
    var feasible = false
    // State the model and solve it
    val cp = CPSolver()
    //cp.silent = true
    // for each facilities, the location chosen for it
    val x: Array[CPVarInt] = Array.fill(n)(CPVarInt(cp, N))
    val dist = Array.tabulate(n,n){case(i,j) => d(x(i))(x(j))}
    val obj1 = sum(N, N)((i, j) => dist(i)(j) * w1(i)(j))
    val obj2 = sum(N, N)((i, j) => dist(i)(j) * w2(i)(j))

    def heuristic(w: Array[Array[Int]]): (Int,Int) = {
      val (weight,i: Int,j: Int) = 
        (for (i <- N; j <- N; if (!x(i).isBound)) 
          yield (w(i)(j) + w(j)(i), i, j)).max
      val (dist, vi: Int) = 
        (for (vi <- x(i); vj <- x(j)) 
          yield (d(vi)(vj) + d(vi)(vj), vi)).min
      (i,vi)
    }

    var i = 0
    cp.addDecisionVariables(x)
    
    
    cp.paretoMinimize(obj1, obj2) subjectTo {
      cp.add(allDifferent(x), Strong)
    } exploration {
      //cp.binaryFirstFail(x, _.randomValue)
      while (!allBounds(x)) {
        val (i,v) = heuristic(/*if(rand.nextInt(2) == 0) w1 else w2*/w1)
        cp.branch(cp.post(x(i) == v))(cp.post(x(i) != v))
      }
      
      plotPareto.insert(obj1.value,obj2.value)
      //hype.addPoint(i, Hypervolume.hypervolume(pareto))
      i += 1
    } run(1)

    val maxRestarts = 5000 // number of restarts
    val maxFailures = 50 // max number of failures at each restart
    val relaxSize = 5 // number of relaxed variables at each restart
    val probaIntensify = 90 // probability (%) to make an intensification restart
    for (i <- 0 until maxRestarts) {
      val sol = randomNonDominatedSol()
      cp.runSubjectTo(Int.MaxValue, maxFailures) {
        if (rand.nextInt(100) < probaIntensify) {
          cp.objective.intensify(sol)          
        } else {
          cp.objective.diversify()
        }
        relaxRandomly(x, sol, relaxSize)
      }
    }
    
    def randomNonDominatedSol(): CPSol = {
      val sols = (for (x <- cp.nonDominatedSolutions) yield x).toArray
      sols(rand.nextInt(sols.size))
    }

   
    // Print some statistics
    cp.printStats()
  }

}

class QAPInitializer()

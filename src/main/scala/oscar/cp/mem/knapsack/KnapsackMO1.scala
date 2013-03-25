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


package oscar.cp.mem.knapsack;

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
 * @author Pierre Schaus pschaus@gmail.com
 */
object KnapsackMO1 extends App {

  
  //val dataFile = "data/mo-knapsack/250.txt"
  //val solutionFile = "data/mo-knapsack/solution/250_2.txt"

  //val dataFile = "data/mo-knapsack/100.txt"
  //val solutionFile = "data/mo-knapsack/solution/100_2.txt"

  val dataFile = "data/mo-knapsack/2KP100A.txt"
  val solutionFile = "data/mo-knapsack/solution/2KP100A.txt"
    
    
  // Read the data
  var (n:Int,capa1:Int,capa2:Int,items1:Array[(Int,Int)],items2:Array[(Int,Int)]) = KnapsackReader.read(dataFile)
  var N = 0 until n
  val perm = N.sortBy(i => (items1(i)._1 + items2(i)._1)/(items1(i)._2 + items2(i)._2))
  

  
  items1 = (0 until n).map(i => items1(perm(i))).toArray
  items2 = (0 until n).map(i => items2(perm(i))).toArray
  /*
  
  val k = 90
  items1 = items1.take(k)
  items2 = items2.take(k)
  n = k
  N = 0 until n
  */
  
  val rand = new scala.util.Random(0)


  
  //val xinit = Array.fill(n)(0)
  
  // Visualization
  val f = new VisualFrame("Knapsack MO",1,2)
  val paretoPlot = new PlotPareto(nbPareto = 2)
  val sol = KnapsackReader.readSolution(solutionFile)
  for ((o1,o2) <- sol) {
    paretoPlot.insert(-o1,-o2,1)
  }
  val hype = new PlotLine("Hype","progress","hype")

  f.createFrame("TSP Objective Function").add(paretoPlot)
  f.createFrame("TSP Objective Function").add(hype)
  f.pack()
  
 
  
  
  
  println("--------------")
  
  {
    var obj1Sol = Int.MaxValue
    var obj2Sol = Int.MaxValue
    var feasible = false
    // State the model and solve it
    val cp = CPSolver()
    //cp.silent = true
    // for each facilities, the location chosen for it
    val x: Array[CPVarBool] = Array.fill(n)(CPVarBool(cp))
    cp.addDecisionVariables(x)
    val capaVar1 = CPVarInt(cp,0 to capa1)
    val capaVar2 = CPVarInt(cp,0 to capa2)
    val profitVar1 = CPVarInt(cp,0 to items1.map(_._2).sum)
    val profitVar2 = CPVarInt(cp,0 to items2.map(_._2).sum)  
    val profitVar1neg = -profitVar1
    val profitVar2neg = -profitVar2
    
    val knapsack1 = binaryKnapsack(x, items1.map(_._2), items1.map(_._1), profitVar1,capaVar1)
    val knapsack2 = binaryKnapsack(x, items2.map(_._2), items2.map(_._1), profitVar2,capaVar2)
    

    
    cp.paretoMinimize(profitVar1neg, profitVar2neg) subjectTo {
      cp.add(knapsack1)
      cp.add(knapsack2)

      /*
      val w1 = Array(8, 25, 21, 6, 27, 8, 9, 27, 5, 16, 21, 13, 15, 20, 15, 7, 23, 9, 25, 7, 25, 20, 10, 23, 9, 24, 23, 24, 7, 10, 4, 20, 22, 28, 15, 16, 17, 26, 29, 27, 16, 19, 25, 16, 18, 21, 11, 6, 2, 7, 9, 3, 5, 3, 14, 4, 24, 4, 30, 11, 14, 29, 13, 27, 7, 7, 4, 14, 4, 18, 2, 15, 6, 11, 29, 2, 23, 8, 8, 3, 6, 18, 23, 11, 7, 20, 11, 2, 10, 18, 7, 18, 19, 19, 22, 15, 24, 21, 21, 29)
      val opt = "1010010010100011010101111101101110110101010001111110111101000010111011111101011110011111111100110110"
      val solopt = opt.split("").filter(_.size == 1).map(_.toInt)
      println(items1.map(_._1).mkString(","))
      println("totweight:"+(0 until n).map(i => solopt(i)*w1(i)).sum)
      println("totweight1:"+(0 until n).map(i => solopt(i)*items1(i)._1).sum)
      println("totweight2:"+(0 until n).map(i => solopt(i)*items2(i)._1).sum)
      println("totprofit1:"+(0 until n).map(i => solopt(i)*items1(i)._2).sum)
      println("totprofit2:"+(0 until n).map(i => solopt(i)*items2(i)._2).sum)
      for (i <- 0 until n) {
        cp.add(x(i) == solopt(i))
      }
      println("init=>"+profitVar1neg+" "+profitVar2neg)
      */
    } exploration {
      cp.binary(x, _.max)      
      obj1Sol = profitVar1neg.value
      obj2Sol = profitVar2neg.value
      println("sol found")
      paretoPlot.insert(obj1Sol,obj2Sol)
    } run(1)

    for (i <- 0 until 50000) {
      val sol = randomSol()
      paretoPlot.highlight(sol(profitVar1neg),sol(profitVar2neg))
      cp.runSubjectTo(Int.MaxValue, 1000) {
        if (i % 2 == 0) {
          cp.objective.diversify()  
        } else {
          cp.objective.intensify(sol)
        }
        relaxRandomly(x, sol, 10)
      }
    }
    
    def randomSol(): CPSol = {
      val sols = (for (x <- cp.nonDominatedSolutions) yield x).toArray
      sols(rand.nextInt(sols.size))
    }
    

    // Print some statistics
    cp.printStats()
  }

}


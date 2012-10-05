/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.core._
import collection.immutable.SortedSet


import collection.mutable.ArrayBuffer

class Calculator(){
  val numbers = new ArrayBuffer[Double]();
  def add(number:Double):Unit = {
    numbers += number;
  }
  def avg():Double = {
      var sum:Double = 0.0;
      for(x:Double <- numbers){
        sum = sum + x;
      }
      sum/numbers.length;
  }
   // Thanks to Christopher Martin
  def avg_short:Double = numbers.reduceLeft(_ + _) / numbers.length

  def stdDev():Double = {
      var sum:Double    = 0.0;
      if(numbers.length>=2){
        val mean = avg();
        val factor:Double = 1.0/(numbers.length.toDouble-1);
        for(x:Double <- numbers){
          sum = sum + ((x-mean)*(x-mean));
        }
        sum = sum * factor;
      }
      return Math.sqrt(sum);
  }
  def print():String = {
    var result:String = "";
    result = result + "Calculator -> ";
    for(x <- numbers) result = result + x +" ";
    return result;
  }
  def reset():Unit = {
    numbers.clear();
  }
}

/**
 * Problem statement :
 * @author Pierre Schaus pschaus@gmail.com
 */
object Roster extends App {
  def test(seed: Int, config: Int): (Double,Double) = {
    val n = 15
    val m = 15
    val maxNumber = n
    val rand = new scala.util.Random(seed)

    val roster = Array.fill(n, m)(rand.nextInt(maxNumber))

    def dom(i: Int, j: Int): Set[Int] = {
      (0 to 5).map(i => rand.nextInt(maxNumber)).toSet
    }

    val cp = CPSolver()
    cp.silent = true
    val x_ = Array.tabulate(n, m)((i, j) => CPVarInt(cp, dom(i, j)))
    val vars = x_.flatten
    
    val violLine = Array.fill(n)(CPVarInt(cp, 0 until m))
    val violCol = Array.fill(m)(CPVarInt(cp, 0 until n))
    val viol = violLine ++ violCol
    
    val bestSolViol = Array.fill(n+m)(0)
    var bestSol = Map[CPVarInt,Int]()
    val relaxedVar: Array[Set[CPVarInt]] = Array.tabulate(n)(i => x_(i).toSet) ++ Array.tabulate(m)(j => (for(i <- 0 until n) yield x_(i)(j)).toSet)
    
    var restartNb = 0
    
    val tabu = Array.fill(viol.size)(-1)
    

    cp.lns(200, 50) {
      restartNb += 1

      if (config == 0) {
        for (x <- vars; if rand.nextInt(100) < 90) {
          cp.post(x == bestSol(x))
        }
      } else {

        val obj = (0 until viol.size).filter(tabu(_) <= restartNb).maxBy(i => (bestSolViol(i),rand.nextInt(n)))
        tabu(obj) = restartNb + rand.nextInt(3)+2
        if (config == 2) cp.objective.currentObjective = obj
        for (x <- vars; if (!relaxedVar(obj).contains(x) && rand.nextInt(100) < 90)) {
            cp.post(x == bestSol(x))
        }
        
      }

    }
    val tot = sum(violLine) + sum(violCol)
    if (config < 2) cp.minimize(sum(viol))
    else cp.minimize(viol: _*)
    cp.subjectTo {
      for (i <- 0 until viol.size) {
        val count = Array.fill(m)(1)
        cp.add(softgcc(relaxedVar(i).toArray, 0 until maxNumber, count, count, viol(i)))
      }
    } exploration {
      cp.binaryFirstFail(vars)

      bestSol = (for (i <- 0 until vars.size) yield (vars(i) -> vars(i).value)).toMap
      for (i <- 0 until viol.size) {
        bestSolViol(i) = viol(i).value
      }
    }
    
    val calc = new Calculator()
    bestSolViol.foreach(calc.add(_))
    (bestSolViol.sum,calc.stdDev())
  }
  
  
  for (i <- 0 until 10) {
    val a = test(i,0)
    val b = test(i,1)
    val c = test(i,2)
    println("========>  average:"+a._1+" "+b._1+" "+c._1+" std:"+a._2+" "+b._2+" "+c._2)
  }
  
  
}

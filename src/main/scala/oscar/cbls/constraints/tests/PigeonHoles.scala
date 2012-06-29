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
 ******************************************************************************/
/*
 * Copyright CETIC 2012 www.cetic.be
 *
 * This file is part of Asteroid.
 *
 * Asteroid is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Asteroid is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Asteroid.
 * If not, see http://www.gnu.org/licenses/lgpl-2.1-standalone.html
 *
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Christophe Ponsard
 */

package oscar.cbls.constraints.tests

import oscar.cbls.search._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.constraints.core._
import oscar.cbls.invariants.core.computation.Implicits._
import oscar.cbls.invariants.lib.numeric.Implicits._
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._
import oscar.cbls.algebra.Implicits._

/**
 * Very simple example showing how to use Asteroid on the basic pigeon hole problem
 * Using constraint system (better alternative: use ArgMax to keep track of violation)
 */
object PigeonHoles extends SearchEngine with StopWatch {
  
  def main(args: Array[String]) {

    if (args.length<2) {
      println("argument: nr_holes nr_pigeons")
      System.exit(0)
    }
    
    startWatch()

    // nr of pigeons
    val N:Int=args(0).toInt

    // nr of holes
    val M:Int=args(1).toInt

    val range:Range = Range(0,M) // note Scala seems to consider 0..M-1 as range

    println("PigeonHoles(" + N + "," + M + ")")

    // model
    val m: Model = new Model(false,false,true)
       
    // holes
    val holes:Array[IntVar] = (for(i <- range) yield new IntVar(m, 0, N, 0, "h" + (i+1))).toArray
    
    // initially all pigeons are in the first hole...
    holes(0).setValue(N)

    // print initial state
    println(holes.toList)
    
    // constraint system (alternative: use ArgMax rather than Constraint System)
    val c:ConstraintSystem = new ConstraintSystem(m)
    
    // requiring all holes to have less that a pigeon
    //val ONE=new IntVar(m,0,N,1, "ONE")
    for (i <- range) {
      c.post(holes(i) <=: 1 )
    }
    // enforcing sum (not required if transfer is used during search)
    
    c.close()
    m.close()

    println("run time after model close: "+ getWatchString)

    var it:Int =0
    val MaxIT = 2*N

    while((c.Violation.getValue() > 0) && (it < MaxIT)){
      val holeMax:(Int)=selectMax(range, (p:Int) => holes(p))
      val holeMin:(Int)=selectMin(range, (p:Int) => holes(p), (p:Int) => p != holeMax )

      holes(holeMax).setValue(holes(holeMax).toInt-1)
      holes(holeMin).setValue(holes(holeMin).toInt+1)
      
      it += 1
      println("it: " + it + " " + c.Violation + " (moved from "+ holeMax + " to " + holeMin + ")")
    }

    println(c.Violation)
    println(m.getSolution(true))
    println("run time: "+ getWatchString)
  }
}

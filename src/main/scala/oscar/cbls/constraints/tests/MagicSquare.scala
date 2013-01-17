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

/*******************************************************************************
 * Contributors:
 *     This code has been initially developed by CETIC www.cetic.be
 *         by Christophe Ponsard and Renaud De Landtsheer
 ******************************************************************************/


package oscar.cbls.constraints.tests

import oscar.cbls.search._
import oscar.cbls.constraints.core._
import oscar.cbls.constraints.lib.global.AllDiff
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.lib.logic._
import oscar.cbls.invariants.lib.minmax._

/**
 * Example showing how to use Asteroid on the magic square problem  
 */
object MagicSquare extends SearchEngine with StopWatch {
  
  def main(args: Array[String]) {
    
    startWatch()
    
    if (args.length<1) {
      println("argument: dimension")
      System.exit(0)
    }
    val N:Int=args(0).toInt
    val M:Int=N*N
    val T:Int=(N*(N*N+1))/2
    val Dim:Range= Range(0,N)
    val Dim2:Range= Range(0,M)
    
    println("Magic Square("+N+") - SUM is: "+T)
    
    // Search control
    val MAX_IT = 10000
    val TABU_LENGTH = N/2+1

    // model
    val m: Model = new Model(false,false,true)
        
    // Square
    val magic = Array.ofDim[IntVar](N,N)
    val perm:Iterator[Int] = getRandomPermutation(M)
    var v:Int=1
    for(i <- Dim; j <-Dim) { 
      magic(i)(j)=new IntVar(m, 1, N, perm.next+1, "v_"+i+"_"+j) // init with random permutation (ensuring all diff)
      v=v+1
    }
    showSquare(magic)
    
    // constraint system
    val c:ConstraintSystem = new ConstraintSystem(m)
    
    // c.post(AllDiff(magic)) // all diff will be maintained by swap
    for(i <- Dim) c.post(EQ(Sum(for ( j <- Dim) yield magic(i)(j)), T)) // lines
    for(j <- Dim) c.post(EQ(Sum(for ( i <- Dim) yield magic(i)(j)), T)) // columns
    c.post(EQ(Sum(for ( i <- Dim) yield magic(i)(i)), T)) // one diagonal
    c.post(EQ(Sum(for ( i <- Dim) yield magic(i)(N-i-1)), T)) // other diagonal
    
    // register for violation
    for (i <- Dim; j <- Dim) { c.violation(magic(i)(j)) }

    // closing constraints
    c.close
    
    // working variables - using flat arrays
    // conversion is: i=v/N, j=v%N
    // TODO - can we work with multidimensional arrays here ?
    //val ViolationArray:Array[IntVar] = (for(i <- Dim; j <- Dim) yield c.violation(magic(i)(j))).toArray
    val Tabu:Array[IntVar] = (for (i <- Dim; j <- Dim) yield new IntVar(m, 0, Int.MaxValue, 0, "Tabu_"+i+"_"+j)).toArray
    var it:Int=1
    
    // closing model
    m.close()
      
    // search
    while((c.Violation.value > 0) && (it < MAX_IT)){
      val allowed = Dim2.filter(v => Tabu(v).value < it)
      val (v1,v2)=selectMin(allowed, allowed) ((v1,v2) => c.swapVal(magic(v1/N)(v1%N),magic(v2/N)(v2%N)), (v1,v2) => v1 < v2)

      val i1=v1/N
      val j1=v1%N
     
      val i2=v2/N
      val j2=v2%N
            
      magic(i1)(j1) :=: magic(i2)(j2)
      Tabu(v1) := it + TABU_LENGTH
      Tabu(v2) := it + TABU_LENGTH

      it=it+1   
      println("it: "+ it + " " + c.Violation + " (swapped "+ magic(i1)(j1) + " and " + magic(i2)(j2) + ")")
    }
     
    println("Solution: "+m.getSolution(true))

    if (c.Violation.value==0) {
      showSquare(magic)    
    } else {
      println("Not found after "+MAX_IT+" iterations")
    }
    
    println("run time: "+ getWatch)
  }

  def showSquare(tab:Array[Array[IntVar]]) {
    println
    for (i <- Range(0,tab.length)) {
      for (j <-Range(0,tab(i).length)) {
        print(tab(i)(j).value+" ")
      }
      println
    }
    println
  }
  
}

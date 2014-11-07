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
/**
 * @author Jean-Noël Monette
 */

package oscar.examples.cbls

import oscar.cbls.search._
import oscar.cbls.constraints.core._
import oscar.cbls.constraints.lib.basic._
import oscar.cbls.invariants.core.computation._
import oscar.cbls.invariants.lib.numeric._
import oscar.cbls.invariants.core.computation.IntInvariant.toIntVar
import oscar.cbls.invariants.core.computation.CBLSIntVar.int2IntVar
import oscar.flatzinc.model.exactly_int
import oscar.cbls.invariants.lib.logic.DenseCount

/**
 * Example showing how to use Asteroid on the magic square problem  
 * @author christophe.ponsard@cetic.be
 * */
object MagicSeries2 extends SearchEngine with StopWatch {
  
  def main(args: Array[String]) {
    //test(8,true);
    val N:Int=if (args.length<1) {
      210
    }else args(0).toInt
    
    println("Magic Series("+N+")")
    
    startWatch()
    println((1 to 100).foldLeft(0)((acc,i) => acc+test(N,true,true)))
    println("run time: "+ getWatch)
    startWatch()
    println((1 to 100).foldLeft(0)((acc,i) => acc+test(N,false,true)))
    println("run time: "+ getWatch)
    startWatch()
    println((1 to 100).foldLeft(0)((acc,i) => acc+test(N,true,false)))
    println("run time: "+ getWatch)
    startWatch()
    println((1 to 100).foldLeft(0)((acc,i) => acc+test(N,false,false)))
    println("run time: "+ getWatch)
    /* Some result
Magic Series(210)
....................................................................................................5014
run time: 42316
....................................................................................................6469
run time: 41268
....................................................................................................4235
run time: 232828
....................................................................................................5849
run time: 255719
     */
  }
  def test(N:Int,red:Boolean,gcc:Boolean): Int = {
    val Size:Range= 0 until N
    
    
    
    // model
    val m: Store = new Store()
    val s = Size.map(i => CBLSIntVar(m,Size,0,"s_"+i)).toArray
    val c = ConstraintSystem(m)
    //
    if(gcc){
      val dc = DenseCount.makeDenseCount(s);
      val counts = dc.counts
      for(v <- Size) c.post(EQ(s(v),counts(v)));
    }else{
      for(v <- Size) c.post(EQ(s(v),Sum(Size.map(i => EQ(s(i),v).toIntVar))))//TODO: atleast+atmost
    }
    if(red)c.post(EQ(Sum(Size.map(i => Prod2(s(i),i).toIntVar)), N)) 
    
    
    val violations = s.map(si => c.violation(si)) 

    
    
    
    m.close()
    
    val tabu = s.map(_ => -1)
    var it:Int=0
    
    // Search control
   // val MAX_IT = 10000
    val TABU_LENGTH = 3

    // search
    while((c.violation.value > 0)){
      
      val i = selectMax(Size,(i:Int) => violations(i).value, (i:Int) => tabu(i) <= it)
      val v = selectMin(Size)((v:Int) => c.assignVal(s(i),v),(v:Int) => s(i).value != v)
     // println("it: "+ it + " " + c.violation + " (assigned "+ s(i) + " to " + v + ")")       
      s(i) := v
      tabu(i) = it + TABU_LENGTH
      it=it+1   
      
    }
     
  //  println("Solution: "+s.map(_.value).mkString("[",",","]"))
    print(it+".")
    it
  }
  
}

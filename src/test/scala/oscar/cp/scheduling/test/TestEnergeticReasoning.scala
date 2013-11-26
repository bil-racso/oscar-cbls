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
package oscar.cp.scheduling.test


import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.modeling._
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest._
import scala.collection.mutable.ArrayBuffer

@RunWith(classOf[JUnitRunner])
class TestEnergeticReasoning extends FunSuite with ShouldMatchers  {

  //example from Baptiste's book
  test("ER : Detect infeasibility") {
    //a task : (startmin, endmax, dur, demand)	
  
	val activitiesDescription = Array((1,8,4,1), (1,8,4,1), (0,10,4,1),(0,10,4,1),(0,10,4,1))
	
	val horizon = 3000
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 1
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 2 to 2)
	
	var nb = 0
	cp.solve subjectTo
	{
		cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
	} exploration {
			cp.binaryFirstFail(starts)
    	    nb += 1
    	} run()
    	
    nb should be(0)

  }  
  
  test("ER : Pruning") {
    //a task : (startmin, endmax, dur, demand)
  
    val activitiesDescription = Array((1,8,4,1), (0,10,4,1),(0,10,4,1))
	
	val horizon = 3000
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 1
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 2 to 2)

	
	cp.add(starts(0) == 1)
	cp.add(starts(1) == 0)
	
	for(i <- 0 until starts.length) {
	  cp.add(starts(i) + durs(i) == ends(i))
	}
	
	cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
    	
	starts(2).getMin should be(4)
	
	

  }  
  
  test("ER : test3") {
    //a task : (startmin, endmax, dur, demand)
  
    val activitiesDescription = Array((0,4,3,1), (0,4,3,1),(0,4,1,2))
	
	val horizon = 4
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 0
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 2 to 2)

	
	var nb = 0
	cp.solve subjectTo
	{
      for(i <- 0 until starts.length) {
	  cp.add(starts(i) + durs(i) == ends(i))
	}
		cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
	} exploration {
			cp.binaryFirstFail(starts)
    	    nb += 1
    	} run()
    	
    nb should be(2)
	

  }
  
  test("ER : zero duration") {
    //a task : (startmin, endmax, dur, demand)
  
    val horizon = 3
    
    val activitiesDescription = Array((0,horizon,2,2), (0,horizon,0,1),(0,horizon,1,3))
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 0
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 4 to 4)

	var nb = 0
	val solutions = new Array[Tuple3[Int,Int,Int]](8)
	
	cp.solve subjectTo
	{
      for(i <- 0 until starts.length) {
	  cp.add(starts(i) + durs(i) == ends(i))
	}
		cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
	} exploration {
			cp.binaryFirstFail(starts)
			solutions(nb) = Tuple3(starts(0).value,starts(1).value,starts(2).value)
    	    nb += 1
    	} run()
    	
    nb should be(8)
    	
    val realSolution = Array{Tuple3(0,0,2); 
    						Tuple3(0,1,2);
    						Tuple3(0,2,2);
    						Tuple3(0,3,2);
    						Tuple3(1,0,0);
    						Tuple3(1,1,0);
    						Tuple3(1,2,0);
    						Tuple3(1,3,0)
    						}	
    realSolution foreach {sol => assert(solutions.contains(sol), s"$sol is a solution")}
	
  }
  
  test("ER : zero duration 2") {
    //a task : (startmin, endmax, dur, demand)
  
    val horizon = 2
    
    val activitiesDescription = Array((0,horizon,0,3), (0,horizon,2,3))
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	val durs = activitiesDescription.map(aD => CPVarInt(cp, aD._3, aD._3))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 0
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 4 to 4)

	var nb = 0
	val solutions = new Array[Tuple2[Int,Int]](3)
	
	cp.solve subjectTo
	{
      for(i <- 0 until starts.length) {
	  cp.add(starts(i) + durs(i) == ends(i))
	}
		cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
	} exploration {
			cp.binaryFirstFail(starts)
			solutions(nb) = Tuple2(starts(0).value,starts(1).value)
    	    nb += 1
    	} run()
    	
    nb should be(3)
    	
    val realSolution = Array{Tuple2(0,0); 
    						Tuple2(1,0);
    						Tuple2(2,0);
    						}	
    realSolution foreach {sol => assert(solutions.contains(sol), s"$sol is a solution")}
	
  }
  
  test("ER : variable duration") {
    //a task : (startmin, endmax, dur, demand)
  
    val horizon = 3
    
    val activitiesDescription = Array((0,horizon,2,2), (0,horizon,0,3))
	
	val cp = CPScheduler(horizon)

	val starts = activitiesDescription.map(aD => CPVarInt(cp, aD._1 to aD._2 - aD._3))
	//second activity has variable duration
	val durs = 0 until activitiesDescription.length map(i => if(i != 1) CPVarInt(cp, activitiesDescription(i)._3 to activitiesDescription(i)._3) else CPVarInt(cp, 0 to 1))
	val ends = activitiesDescription.map(aD => CPVarInt(cp, aD._1 + aD._3 to aD._2))
	val demands = activitiesDescription.map(aD => CPVarInt(cp, aD._4 to aD._4))
	
	val resId = 0
	
	val resources = Array.fill(activitiesDescription.length)(CPVarInt(cp, resId to resId)) 

	val capa = CPVarInt(cp, 4 to 4)

	var nb = 0
	val solutions = new ArrayBuffer[Tuple3[Int,Int,Int]]()
	
	cp.solve subjectTo
	{
      for(i <- 0 until starts.length) {
	  cp.add(starts(i) + durs(i) == ends(i))
	}
		cp.add(new EnergeticReasoning(starts,durs,ends,demands,resources,capa,resId))
	} exploration {
			cp.binaryFirstFail(starts)
			cp.binaryFirstFail(durs)
			solutions += Tuple3(starts(0).value,starts(1).value,durs(1).value)
    	    nb += 1
    	} run()

    	
    	solutions foreach {println(_)}
    	
    nb should be(10)
    
    val realSolution = Array{
    				Tuple3(0,0,0)
    				Tuple3(0,1,0)
    				Tuple3(0,2,0)
    				Tuple3(0,2,1)
    				Tuple3(0,3,0)
    				Tuple3(1,0,0)
    				Tuple3(1,0,1)
    				Tuple3(1,1,0)
    				Tuple3(1,2,0)
    				Tuple3(1,3,0)
    }
    	
    realSolution foreach {sol => assert(solutions.contains(sol), s"$sol is a solution")}
	
  }

  
}

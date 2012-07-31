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

/*package oscar.examples.cp.scheduling

import oscar.cp.modeling._
import oscar.search._
import oscar.cp.scheduling.CumulativeActivity
import oscar.cp.constraints._
import oscar.cp.core.CPVarInt

/**
 *  Example taken from the minizink competition: 
 *  http://www.g12.cs.mu.oz.au/minizinc/challenge2011/probs/cyclic-rcpsp/rcmsp.mzn
 *  Model example for Resource-Constrained Modulo Scheduling Problems (RCMSP)
 *  RCMSP is a cyclic resource-constrained project scheduling problem with 
 *  generalized precedence relations  constrained to scarce cumulative resources 
 *  and tasks which are repeated infinitely. These tasks need some resource units 
 *  from the resource for their executions, but the resource cannot be overloaded
 *  at any point in time. Moreover, the tasks have an unit durations, except the
 *  artificial source and sink tasks which have a zero duration.
 *  The factor which a task is repeated is called period. Here, the periods of
 *  all tasks are the same.
 *  
 *  The generalized precedence relations, i.e., minimal and maximal time lags, are
 *  of form of s[i, k] + lat[i, j] <= s[j, k + w[i,j]], where s[i, k] is the
 *  start time of task i in the k's iteration, lat[i, j] is the latency between
 *  task i and task j, and w[i, j] is the distance between task i and task j.
 *  
 *  As the start times at iteration 0 might be greater than the modulus, we can 
 *  decompose it as: start (i, 0) = si + ki * lambda, where si is the start 
 *  time within the modulus (0 <= si <= lambda - di) and ki, 
 *  called iteration number, refers to the number of full periods 
 *  elapsed before start(i,0) is scheduled.
 * 
 *      lambda
 *       |---|
 *       ____________
 *      |____    ____|         
 *        __ |__|
 *   ____|  |____
 *  |____________|
 *  
 *  A constraint based approach to cyclic RCPSP, CP2011 (Alessio Bonfietti, Michele Lombardi, Luca Benini, and Michela Milano
 * val lines = scala.io.Source.fromFile("prec.txt").getLines().toArray
 * lines.zipWithIndex.foreach{case(l,i) => println("<task id=\""+(i+1)+"\" "+l.trim.split(",").map(_.trim).zipWithIndex.map{case(v,i) => "req"+(i+1)+"=\""+v+"\""}.mkString(" ")+"/>")}
 * lines.zipWithIndex.foreach{case(l,i) => println("<precedence id=\""+(i+1)+"\" "+Array("t1","t2","latency","distance").zip(l.trim.split(",")).map{case(a,b) => a+"=\""+b.trim+"\""}.mkString(" ")+"/>")} 
 * 
 *  @authors: Pierre Schaus pschaus@gmail.com
 */
object CyclicRCPSP {
  
	def main(args: Array[String]) {
       
		val problemNode = xml.XML.loadFile("data/rcpsp-easy4.xml")
      
		val capa = for (node <- (problemNode \ "resources" \ "resource").toArray) yield (node \ "@capa").text.toInt
    	
		val req = for (node <- (problemNode \ "tasks" \ "task").toArray) yield {
			(1 to capa.size).map(i => (node \ ("@req"+i)).text.toInt)
		}
      
		val prec = for (node <- (problemNode \ "precedences" \ "precedence").toArray) yield {
			((node \ "@t1").text.toInt-1,(node \ "@t2").text.toInt-1,(node \ "@latency").text.toInt,(node \ "@distance").text.toInt)
		}    
      
		val nTasks = req.size
		val Tasks = 1 until nTasks-1
		val last = nTasks-1
		val t_max = prec.map(_._4).sum  // Maximal period
		val Times = 0 to t_max
		val Iters = 0 to nTasks
		val Res = 0 until capa.size
		val d = Array.tabulate(nTasks)(i => if (i == 0 || i == last) 0 else 1)
  
		val cp = CPSolver()
		val s = Array.fill(nTasks)(CPVarInt(cp, Times)) // Start time variables
		val k = Array.fill(nTasks)(CPVarInt(cp, Iters)) // Iteration variables
		      
		// Makespan of the one iteration

		val makespan = maximum(Tasks)(i => s(i) - k(i) * s(last)) - minimum(Tasks)(i => s(i) - k(i)* s(last)) + 1
	 
	 /*
	 cp.add()
    max([ s[i] - k[i] * s[n_tasks] | i in Tasks where i > 0 /\ i < n_tasks ])
    - min([ s[i] - k[i] * s[n_tasks] | i in Tasks where i > 0 /\ i < n_tasks ])
    + 1;*/
	 
	 // The objective:
     //   1. minimize the period of the schedule, i.e., "s[n_tasks]"
     //   2. minimize the makespan of one iteration, i.e., "makespan"
		var objective = s(last) * t_max + makespan;

		cp.minimize(objective) subjectTo {
        // Generalised Precedence Constraints
        /*
        forall(i in Prec)(
        let { var bool: b } in (
            ( b <-> ( s[prec[i,1]] + prec[i,3] <= s[prec[i,2]] ) )
        /\  k[prec[i,1]] + bool2int(not(b)) <= k[prec[i,2]] + prec[i,4]
        )
        )
        */
			
        for((act1,act2,latency,distance) <- prec) {

        	val b = s(act1) + latency <== s(act2)
        	cp.add(k(act1) + !b <= k(act2) + distance)
        }
        
        // Redundant non-overlapping constraints
        for (i <- Tasks; j <- Tasks; if (i < j)) {
          if (Res.exists(r => req(i)(r) + req(j)(r) > capa(r))) {
            cp.add((s(i) + d(i) <== s(j)) || (s(j) + d(j) <== s(i)))
          }
        }
        
        
        // Period constraints
        cp.add(maximum(Tasks.map(k(_)),k(last)))
        for (i <- Tasks) {
          cp.add(s(i) + d(i) <= s(last))
        }
        
        // Symmetry breaking constraint

        cp.add(s(0) == 0)
        cp.add(k(0) == 0)
        
        // Cumulative resource constraints
        
        for (r <- Res) {
          val sel = Tasks.filter(i => req(i)(r) > 0 && d(i) > 0)
          if (!sel.isEmpty) {
            val sumReq = sel.map(i => req(i)(r)).sum
            if (sumReq > capa(r)) {
              val act = sel.map(i => CumulativeActivity(s(i),1,0,req(i)(r)))
              cp.add(new MaxSweepCumulative (cp, act.toArray,capa(r),0))
            }
          }
       }
      } exploration {
        //cp.binaryFirstFail(s)
        //cp.binaryFirstFail(k)
        cp.binary(s)
        cp.binary(k)
        //cp.printStats()
      }
      
      
      
     /* 

      println(capa.mkString(","))
      req.foreach(l => println(l.mkString(",")))
      prec.foreach(l => println(l))
  	*/
  }
}*/
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

package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._
import oscar.search._
import oscar.cp.scheduling._
import oscar.visual._
import scala.io.Source

/**
 * @author Pierre Schaus pschaus@gmail.com
 * 
 * Model for the classical nxn Job-Shop problem
 * 
 * A Job is a a sequence of n Activities that must be executed one after the others.
 * There are n machines and each activity of the jobs require one of the n machines.
 * The objective is to assign the starting time of each activity minimizing the total makespan and
 * such that no two activities from two different jobs requiring the same machine overlap.
 */
object JobShop  extends CPModel {
	def main(args: Array[String]) {
	  

		// Read the data
	   var lines = Source.fromFile("data/ft10.txt").getLines.toList
	   val n  = lines.head.trim().split(" ")(0).toInt // number jobs
	   val m  = lines.head.trim().split(" ")(1).toInt // number machines and activities per job
	   val cols = VisualUtil.getRandomColorArray(m)
	   lines = lines.drop(1)
	   val durations : Array[Array[Int]] = Array.fill(n,m)(0)
	   val machines : Array[Array[Int]] = Array.fill(n,m)(0)
	   for (i <- 0 until n) {
	  	   val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
	  	   println(m+" "+n+" "+l.mkString(" ")+"  size: "+l.size)
	  	   machines(i) = Array.tabulate(m)(j => l(2*j))
	  	   durations(i) = Array.tabulate(m)(j => l(2*j+1))
	  	   lines = lines.drop(1)
	   }
	   val horizon = durations.flatten.sum
	   
	   // Create solver, activities, resources and variables
	   val cp = CPSolver()
	   class JobShopAct(val act : Activity,val job: Int, val machine : Int)
	   
	   //makespanLine.setInnerCol(java.awt.Color.red)
	   val activities = Array.tabulate(n,m)((i,j) => {
	  	   									 val dur = durations(i)(j)
	  	   									 val machine = machines(i)(j)
	  	   									 val start = CPVarInt(cp,0 to horizon-dur)
	                                         new JobShopAct(new Activity(start,dur),i,machine) }) 	   
	   val activitiesMachine  =  Array.tabulate(m)(m => activities.flatten.filter(_.machine == m).map(_.act ).toArray)                              	   
	   val makespan = maximum(0 until n)(i => activities(i)(m-1).act.getEnd)       
       var resources = Array.tabulate(m)(i => unaryResource(activitiesMachine(i)))
             
       
       // -----------      visualization --------
       val f = new VisualFrame("JobShop",2,1)
	   val drawing = new VisualDrawing(false)
	   val mdrawing = new VisualDrawing(false)
	   val scale_y = 30
	   val scale_x = 1
	   f.createFrame("Jobs").add(drawing)
	   f.createFrame("Machines").add(mdrawing)
	   f.pack()
	   drawing.repaint()
	   val visualAct: Array[Array[VisualRectangle]] = Array.tabulate(n,m){(i,j) => val activity = activities(i)(j)
	                                                val rect = new VisualRectangle(drawing,0,scale_y*activity.job, activity.act.getMinDuration()*scale_x,scale_y)
	   												rect.setInnerCol(cols(activity.machine))
	   												rect }
	   val machineAct: Array[Array[VisualRectangle]] = Array.tabulate(m){ m => 
	     												activitiesMachine(m).map{ a =>
	     													val rect = new VisualRectangle(mdrawing,0,scale_y*m, a.getMinDuration()*scale_x,scale_y)
	     													rect.setInnerCol(cols(m))
	     													rect }}
	   
	   
	   val makespanLine = new VisualLine(drawing,0,0,0,n*scale_y)
	   
	   def updateVisu() = {
	     for (i <- 0 until n; j <- 0 until m) {	       
	    	 visualAct(i)(j).move(activities(i)(j).act.getStart().getMin()*scale_x,scale_y*activities(i)(j).job)
	     }
	     for (i <- 0 until m; j <- 0 until activitiesMachine(i).size) {
	        val act = activitiesMachine(i)(j)
	        machineAct(i)(j).move(act.getStart().getMin()*scale_x,scale_y*i)
	       
	     }
	     makespanLine.setOrig(makespan.getMin()*scale_x,0)
	     makespanLine.setDest(makespan.getMin()*scale_x,n*scale_y)
	   }
	   
	   // ---------------------------------------
       
       
       cp.minimize(makespan) subjectTo {
	  	   // add the precedence constraints inside a job
	  	   for (i <- 0 until n; j <- 0 until m-1) {
	  	  	   cp.add(activities(i)(j).act.getEnd() <= activities(i)(j+1).act.getStart())
	  	   }
	  	   // add the unary resources
	  	   resources.foreach(r => cp.add(r))  	   	   
	   } exploration {
		 println("cricicality:"+(0 until m).map(resources(_).getCriticality()).mkString(","))
		 // search
	     for (r <- (0 until m).sortBy(-resources(_).getCriticality()).suspendable) {
	       // cp.binaryFirstFail(resources(i).getRanks())
	       val activs = resources(r).getActivities()
	       while (!resources(r).isRanked()) {
	         val toRank = (0 until activs.size).filter(!resources(r).isRanked(_)).sortBy(i => (activs(i).getEST(),activs(i).getLST()))
	         // try all not yet ranked activities as the first one to rank
	         cp.branchAll(toRank)(i => resources(r).rankFirst(i))
	       }
	     }
	     println("all ranked")
	     val min = makespan.getMin()
	     println("try min makespan:"+min)
	     cp.binary(Array(makespan))
	     println("makespan fixed to:"+min+" makespan:"+makespan)
	     //cp.binaryFirstFail(activities.flatten.map(_.act.getStart))
	     // sol found, update the visu
	     updateVisu()
	     cp.printStats()

	   }
       cp.printStats()
    }
}

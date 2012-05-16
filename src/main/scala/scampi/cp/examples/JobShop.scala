/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.examples


import scampi.cp.modeling._
import scampi.cp.core._
import scampi.search._
import scampi.cp.scheduling._
import scampi.visual._
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
	   var lines = Source.fromFile("data/ft07.txt").getLines.toList
	   val n  = lines.head.trim().split(" ")(0).toInt
	   val cols = VisualUtil.getRandomColorArray(n)
	   lines = lines.drop(1)
	   val durations : Array[Array[Int]] = Array.tabulate(n)(_ => null)
	   val machines : Array[Array[Int]] = Array.tabulate(n)(_ => null)
	   for (i <- 0 until n) {
	  	   val l = lines.head.trim().split("[ ,\t]+").map(_.toInt).toArray
	  	   machines(i) = Array.tabulate(n)(j => l(2*j))
	  	   durations(i) = Array.tabulate(n)(j => l(2*j+1))
	  	   lines = lines.drop(1)
	   }
	   val horizon = durations.flatten.sum
	   
	   // Create solver, activities, resources and variables
	   val cp = CPSolver()
	   class JobShopAct(val act : Activity,val job: Int, val machine : Int)
	   
	   //makespanLine.setInnerCol(java.awt.Color.red)
	   val activities = Array.tabulate(n,n)((i,j) => {
	  	   									 val dur = durations(i)(j)
	  	   									 val machine = machines(i)(j)
	  	   									 val start = CPVarInt(cp,0 to horizon-dur)
	                                         new JobShopAct(new Activity(start,dur),i,machine) }) 	   
	   val activitiesMachine  =  Array.tabulate(n)(m => activities.flatten.filter(_.machine == m).map(_.act ).toArray)                              	   
	   val makespan = maximum(0 until n)(i => activities(i)(n-1).act.getEnd)       
       var resources = Array.tabulate(n)(i => unaryResource(activitiesMachine(i)))
             
       
       // -----------      visualization --------
       val f = new VisualFrame("JobShop",1,1)
	   val drawing = new VisualDrawing(false)
	   val scale_y = 30
	   val scale_x = 1
	   f.createFrame("JobShop").add(drawing)
	   f.pack()
	   drawing.repaint()
	   val visualAct: Array[Array[VisualRectangle]] = Array.tabulate(n,n){(i,j) => val activity = activities(i)(j)
	                                                val rect = new VisualRectangle(drawing,0,scale_y*activity.job, activity.act.getMinDuration()*scale_x,scale_y)
	   												rect.setInnerCol(cols(activity.machine))
	   												rect }
	   val makespanLine = new VisualLine(drawing,0,0,0,n*scale_y)
	   def updateVisu() = {
	     for (i <- 0 until n; j <- 0 until n) {	       
	    	 visualAct(i)(j).move(activities(i)(j).act.getStart().getMin()*scale_x,scale_y*activities(i)(j).job)
	     }
	     makespanLine.setOrig(makespan.getMin()*scale_x,0)
	     makespanLine.setDest(makespan.getMin()*scale_x,n*scale_y)
	   }
	   
	   // ---------------------------------------
       
       
       cp.minimize(makespan) subjectTo {
	  	   // add the precedence constraints inside a job
	  	   for (i <- 0 until n; j <- 0 until n-1) {
	  	  	   cp.add(activities(i)(j).act.getEnd() <= activities(i)(j+1).act.getStart())
	  	   }
	  	   // add the unary resources
	  	   resources.foreach(r => cp.add(r))  	   	   
	   } exploration {
		 // search
	     for (i <- (0 until n).suspendable) {
	       cp.binaryFirstFail(resources(i).getRanks())
	     }
	     val min = makespan.getMin()
	     cp.branch(cp.post(makespan == min))(cp.post(makespan > min))
	     cp.binaryFirstFail(activities.flatten.map(_.act.getStart))
	     // sol found, update the visu
	     updateVisu()

	   }
       cp.printStats()
    }
}
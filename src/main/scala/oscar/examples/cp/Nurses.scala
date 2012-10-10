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
import oscar.search._
import oscar.cp.core._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._

/**
 * Balancing Nurse Problem
 * @author Pierre Schaus pschaus@gmail.com
 */
object Nurses extends App  {
  
  // --- reading the data ---

  val lines = Source.fromFile("data/nurses/bench2/instance0.txt").getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index-1)
  }
  
  val nbZones = next()
  val nbNurses = next()
  println("nbZones:"+nbZones+" nbNurses:"+nbNurses)
  val acuityByZone = Array.fill(nbZones)(Array[Int]())
  val nbPatientsInZone = Array.fill(nbZones)(0)
  for (i <- 0 until nbZones) {
    nbPatientsInZone(i) = next()
    acuityByZone(i) = Array.fill(nbPatientsInZone(i))(next())
  }
  val nbPatients = nbPatientsInZone.sum
  val acuity = acuityByZone.flatten
  
 // ---   find the number of nurses in each zone  ---
  
 val totAcuityInZone = Array.tabulate(nbZones)(i => acuityByZone(i).sum)
 val totAcuity = acuity.sum
 val nbNursesInZone = Array.fill(nbZones)(1)
 
 def H(i: Int): Double = totAcuityInZone(i)*totAcuityInZone(i)
 def delta(i: Int) = H(i)/nbNursesInZone(i) - H(i)/(nbNursesInZone(i)+1)
 
 // progressively increase the number of nurses
 for (k <- nbZones until nbNurses) { 
   selectMax(0 until nbZones)(delta) {
     i => nbNursesInZone(i) += 1
   }
 }
 println("#nurses in each zones:"+nbNursesInZone.mkString(","))
 
 
 // --- ---
 
 val nurses = 0 until nbNurses
 val patients = 0 until nbPatients
 
 val minNbPatientsByZone = Array.tabulate(nbZones) {
   i => Array.fill(nbNursesInZone(i))(1)
 }
 val maxNbPatientsByZone = Array.tabulate(nbZones) {
   i => Array.fill(nbNursesInZone(i))(3)
 }
 
 // --- model ---
 
 val f = new VisualFrame("Steel Mill Slab")
 val colors = VisualUtil.getRandomColorArray(nbZones)
 val drawing: VisualBinPacking = new VisualBinPacking(nbNurses,40)    
 f.createFrame("Nurses").add(drawing)
 
 val scale = 2
 
 for (i <- 0 until nbZones) {
     
   val items = Array.tabulate(nbPatientsInZone(i))(j => drawing.addItem(i,scale*acuityByZone(i)(j)))
 

   
   
   
   val cp = CPSolver()
   val spreadAcuity = CPVarInt(cp,0 to 1000000)
   val nurseOfPatient = Array.fill(nbPatientsInZone(i))(CPVarInt(cp,0 until nbNursesInZone(i)))
   val acuityOfNurse = Array.fill(nbNursesInZone(i))(CPVarInt(cp,1 to 105))
   
   var best = Int.MaxValue
   // each nurse can have at most 3 and at least one patient
   cp.minimize(spreadAcuity) subjectTo {
     cp.add(new oscar.cp.constraints.Spread(acuityOfNurse,acuityByZone(i).sum,spreadAcuity))
     cp.add(gcc(nurseOfPatient,0 until nbNursesInZone(i),1,3))
     cp.add(binpacking(nurseOfPatient,acuityByZone(i),acuityOfNurse))
   } exploration {
     val x = nurseOfPatient
     while (!allBounds(x)) {
		    val bound = x.filter(_.isBound)
		    val maxUsed = if (bound.isEmpty) -1 else bound.map(_.value).max
		    val (y,o) = minDomNotbound(x).head // retrieve the var and its index in x with smallest domain
		    val v = y.min
		    if (v > maxUsed) { // o can only be placed in an empty slab (=> dynamic break of symmetries)
		      cp.branchOne(cp.post(y == v))
		    }
		    else  {
		      cp.branch(cp.post(y == v))(cp.post(y != v))
		    }
     }
     x.zipWithIndex.foreach{case(n,j) => items(j).setBin(n.value + nbNursesInZone.take(i).sum)}
     
     best = spreadAcuity.value
   }
   println("spread zone:"+i+"="+best)
   cp.printStats
  
 }
 


 
 	
      
	
}

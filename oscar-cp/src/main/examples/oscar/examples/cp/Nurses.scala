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
package oscar.examples.cp

import oscar.cp.modeling._
import oscar.cp.core._
import scala.io.Source
import scala.io.Source
import oscar.util._
import oscar.visual._

/**
 * Load Balancing Nurse Allocation Problem
 * 
 * See: 
 * a) Scalable load balancing in nurse to patient assignment problems (Pierre Schaus, Pascal Van Hentenryck, Jean-Charles Regin), CPAIOR-09
 * b) Bound-Consistent Spread Constraint, Application to load balancing in nurse-to-patient assignments (Pierre Schaus,Jean-Charles Regin). Accepted to EURO Journal on Computational Optimization, 2013.
 * 
 * A two step decomposition is used to solve this problem:
 * 1) compute the number of nurses in each zone of the hospital
 * 2) solve each zone independently minimizing the spread of the loads (sum of squares)
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object Nurses extends App  {
  
  // --- reading the data ---
	
  val lines = Source.fromFile("data/nurses/6zones.txt").getLines.reduceLeft(_ + " " + _)
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
 // how much does it increase if we add delta nurses in zone i ?
 def delta(i: Int,delta: Int = 1) =     H(i)/(nbNursesInZone(i)+delta) - H(i)/nbNursesInZone(i)
 
 // compute lower bound to prove our decomposition will eventually be optimal
 def deltaSwap(i: Int,j: Int) = {
    if (nbNursesInZone(i) > 1) {
    	delta(i,-1)+delta(j,+1)
    } else {
      Int.MaxValue
    }	
  }
 
 // progressively increase the number of nurses
 for (k <- nbZones until nbNurses) { 
   val r: IndexedSeq[Int] = 0 until nbZones
   val i = selectMin(r)()(delta(_,1)).get 
   nbNursesInZone(i) += 1
   
 }
 println("---------------------------------------------")
 val lb = (0 until nbZones).map(z => (totAcuityInZone(z).toDouble*totAcuityInZone(z)/nbNursesInZone(z))).sum
 var lb2 = 0.0
 val couples = for (z1 <- 0 until nbZones; z2 <- 0 until nbZones; if (z1 != z2)) yield (z1,z2);
 def swap(t:(Int,Int)) = deltaSwap(t._1,t._2)
 val (i,j) = selectMin(couples)()(swap).get
 println("===============>"+(i,j))
 nbNursesInZone(i) -= 1
 nbNursesInZone(j) += 1
 lb2 = (0 until nbZones).map(z => (totAcuityInZone(z).toDouble*totAcuityInZone(z)/nbNursesInZone(z))).sum
 nbNursesInZone(i) += 1
 nbNursesInZone(j) -= 1
     
   
 println("#nurses in each zones:"+nbNursesInZone.mkString(","))
 
 
 // --- ---
 
 val nurses = 0 until nbNurses
 val patients = 0 until nbPatients
 
 val minNbPatientsByZone = Array.tabulate(nbZones) { i => Array.fill(nbNursesInZone(i))(1) }
 val maxNbPatientsByZone = Array.tabulate(nbZones) { i => Array.fill(nbNursesInZone(i))(3) }
 
 // --- model ---
 
 val f = VisualFrame("Steel Mill Slab")
 val colors = VisualUtil.getRandomColors(nbZones, true)
 colors(0) = java.awt.Color.GREEN
 colors(1) = java.awt.Color.RED
 val drawing: VisualBinPacking = VisualBinPacking(binWidth = 10)    
 f.createFrame("Nurses").add(drawing)
 
 val scale = 3
 var totSpread = 0
 for (i <- 0 until nbZones) {
     
   val items = Array.tabulate(nbPatientsInZone(i))(j => drawing.addItem(i,scale*acuityByZone(i)(j)))
   items.foreach(_.innerCol = colors(i))
   
   // actual cp model solving zone i
 
   val cp = CPSolver()
   cp.silent = true
   val spreadAcuity = CPVarInt(cp,0 to 10e6.toInt)
   val nurseOfPatient = Array.fill(nbPatientsInZone(i))(CPVarInt(cp,0 until nbNursesInZone(i)))
   val acuityOfNurse = Array.fill(nbNursesInZone(i))(CPVarInt(cp,1 to 105))
   println("spreadacuity:"+spreadAcuity)
   var best = Int.MaxValue
   
   cp.onSolution {
     // update the visualization
     nurseOfPatient.zipWithIndex.foreach{case(n,j) => items(j).bin = (n.value + nbNursesInZone.take(i).sum)}
     // store the best objective
     best = spreadAcuity.value
   }
   
   
   cp.minimize(spreadAcuity) subjectTo {
     // spread constraint to compute the objective 
     cp.add(spread(acuityOfNurse,acuityByZone(i).sum,spreadAcuity))
     // each nurse can have at most 3 and at least one patient
     cp.add(gcc(nurseOfPatient,0 until nbNursesInZone(i),1,3))
     // bin-packing to compute the load of each nurse
     cp.add(binpacking(nurseOfPatient,acuityByZone(i),acuityOfNurse))

    } search {
      // first fail, take the patient with smallest domain not yet bound to a nurse
      selectMin(nurseOfPatient)(x => !x.isBound)(x => x.size) match {
        case None => noAlternative // every patient is bound, no child nodes
        case Some(y) => { // ok y is patient not yet assigned to a nurse
          val maxUsed = nurseOfPatient.maxBoundOrElse(-1)
          // try every possible nurses up to the maximum one without any patient less (dynamic symmetry breaking)
          branchAll(0 to maxUsed + 1)(v => cp.post(y == v))
        }
      }
    } 
   val stat = cp.start()
   totSpread += best
   println("spread zone:"+i+"="+best)
   println(stat)
  
 }
 println("---------------------------")
 println("tot spread:"+totSpread+" ?>=? "+lb2 + "optimal:?"+(totSpread < lb2))
 println("lower bound:"+lb)
 
	
}

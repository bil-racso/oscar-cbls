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
import oscar.algo.search._
import oscar.cp.core._
import collection.immutable.SortedSet
/**
 * Problem statement :
 * One evening there was a murder in the home of married couple, their son and daughter. 
 * One of these four people murdered one of the others. 
 * One of the members of the family witnessed the crime.
 * The other one helped the murderer.
 * These are the things we know for sure:
 * 
 * 1. The witness and the one who helped the murderer were not of the same sex.
 * 2. The oldest person and the witness were not of the same sex.
 * 3. The youngest person and the victim were not of the same sex.
 * 4. The one who helped the murderer was older than the victim.
 * 5. The father was the oldest member of the family.
 * 6. The murderer was not the youngest member of the family.
 * 
 * Who was the murderer?
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object MurderMystery extends App {

    val cp = CPSolver()
    
    // data
    val Array(son,daughter,father,mother) = (0 to 3).toArray
    val name = Array("son","daughter","father","mother")
    val sex = Array(0,1,0,1) // 0 = male, 1 = female
    
    
    // variables
    val personWithAge = Array.fill(4)(CPVarInt(cp, 0 to 3)) // personWithAge(i) is younger than personWithAge(i+1)
    val age = Array.fill(4)(CPVarInt(cp, 0 to 3))  // age(i) is the age of person i
    val Array(murderer,witness,helper,victim) = Array.fill(4)(CPVarInt(cp,0 to 3))
    val oldest = personWithAge(3)
    val youngest = personWithAge(0)
    
    
    cp.solve subjectTo {

      cp.add(allDifferent(Array(murderer,witness,helper,victim)),Strong)
      cp.add(allDifferent(age),Strong)
            
      // 1. The witness and the one who helped the murderer were not of the same sex.
      cp.add(sex(witness) != sex(helper))
      
      // 2. The oldest person and the witness were not of the same sex.
      cp.add(sex(oldest) != sex(witness))
      
      // 3. The youngest person and the victim were not of the same sex.
      cp.add(sex(youngest) != sex(victim))
      
      // 4. The one who helped the murderer was older than the victim.
      for (i <- 0 to 3) {
        cp.add(age(personWithAge(i)) == i)
      }
      cp.add(age(helper) > age(victim))
		 
      // 5. The father was the oldest member of the family.
      cp.add(oldest == father)
      cp.add(personWithAge(2) == mother)
      
      // 6. The murderer was not the youngest member of the family.
      cp.add(youngest != murderer)
      
    } search {
      binaryFirstFail(Array(murderer,witness,helper,victim))
    } onSolution {
      println("murderer:"+name(murderer.value)+" witness:"+name(witness.value)+" helper:"+name(helper.value)+" victim:"+name(victim.value)+" youngest:"+name(youngest.value))
    }
    println(cp.start())
}

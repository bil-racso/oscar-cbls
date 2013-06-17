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


package oscar.cp.core

import oscar.reversible.ReversibleSubsetIndexedArray
import oscar.cp.core.CPOutcome._


/**
 * @author Pierre Schaus
 */
class SetDomain(s: CPStore, min: Int, max: Int) {
  
   val values = new ReversibleSubsetIndexedArray(s,min,max)
   
   def requires(value: Int): CPOutcome = {
     if (!values.isPossible(value)) Failure
     else {
       values.requires(value)
       Suspend
     }
   }
   
   
   def excludes(value: Int): CPOutcome = {
     if (values.isRequired(value)) Failure
     else {
       values.excludes(value)
       Suspend
     }
   }
   
   def possibleSize = values.possibleSize
   def requiredSize = values.requiredSize
   
   def isPossible(v: Int) = values.isPossible(v)
   def isRequired(v: Int) = values.isRequired(v)
   
   def possibleSet = values.possibleSet
   def requiredSet = values.requiredSet
	
}

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

package oscar.cp.constraints;


import oscar.cp.core.CPOutcome;
import oscar.cp.core.CPPropagStrength;
import oscar.cp.core.CPVarInt;
import oscar.cp.core.Constraint;
import oscar.cp.util.ArrayUtils;

/**
 * Element Constraint on a 2D array
 * @author Pierre Schaus pschaus@gmail.com
 */
object ElementCst2D {
  
  private var prevT: Array[Array[Int]] = null
  private var prevData: TableData = null
  
  def apply(T: Array[Array[Int]], x: CPVarInt, y: CPVarInt , z:CPVarInt) = {
    if (prevT != T) {
			prevT = T;
			prevData = new TableData(3);
			for (i <- 0 until T.size; j <- 0 until T(i).size) {
				prevData.add(i,j,T(i)(j));
			}
	}
    new TableAC5TCRecomp(prevData,x,y,z)
  }
}
	


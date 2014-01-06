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
package oscar.cp.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.modeling._


class TestDeviationConstraint extends FunSuite with ShouldMatchers  {

  
    test ("testDeviation10") {
    	 val nbDevi = nbSol(false);
    	 val nbDecomp = nbSol(true);
         nbDevi should be(nbDecomp)
    }


    def nbSol(decomp: Boolean): Int = {
        val cp = CPSolver()
        val s = 74
    	val x = Array(CPVarInt(11 to 16)(cp), CPVarInt(9 to 11)(cp), CPVarInt(12 to 14)(cp), CPVarInt(13 to 14)(cp), CPVarInt(10 to 12)(cp), CPVarInt(12 to 15)(cp))
    	val nd = CPVarInt(0 to 34)(cp)

        if (decomp)
        	deviationDecomp(x,s,nd);
        else
        	cp.add(new Deviation(x, s, nd));

        var cnt = 0
        cp.search {
          binaryFirstFail(x)
        } 
        cp.start().nbSols
    }

    def deviationDecomp(x: Array[CPVarInt], s: Int, nd: CPVarInt) {
        val cp = x(0).store;
        val dev: Array[CPVarInt] = Array.tabulate(x.length)(i => x(i).mul(x.length).minus(s).abs())
		cp.post(new Sum(dev, nd))
        cp.post(new Sum(x, CPVarInt(s)(cp)))
    }
 
}

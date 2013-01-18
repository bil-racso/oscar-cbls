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


    def nbSol(decomp: Boolean) = {
        val cp = CPSolver()
        val s = 74
    	val x = Array(CPVarInt(cp,11 to 16),CPVarInt(cp,9 to 11),CPVarInt(cp,12 to 14),CPVarInt(cp,13 to 14),CPVarInt(cp,10 to 12),CPVarInt(cp,12 to 15))
    	val nd = CPVarInt(cp,0 to 34)

        if (decomp)
        	deviationDecomp(x,s,nd);
        else
        	cp.add(new Deviation(x, s, nd));

        var cnt = 0
        cp.solve exploration {
          cp.binaryFirstFail(x)
          cnt += 1
        } run
        cnt
    }

    def deviationDecomp(x: Array[CPVarInt], s: Int, nd: CPVarInt) {
        val cp = x(0).store;
        val dev: Array[CPVarInt] = Array.tabulate(x.length)(i => x(i).mul(x.length).minus(s).abs())
		cp.post(new Sum(dev, nd))
        cp.post(new Sum(x,s))
    }
 
}

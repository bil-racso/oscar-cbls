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


import scala.math

/**
 * Martin Garner Problem:
 * Let a,b,c,d,e,f,g and h be distinct elements in the set {-7,-5,-3,-2,2,4,6,13}
 * What is the minimum possible value of (a+b+c+d)^2 + (e+f+g+h)^2
 * @author Pierre Schaus pschaus@gmail.com
 */
object GardnerSumSquare extends CPModel {

  def main(args: Array[String]) {
    val n = 5
    
    val dom = Set(-7,-5,-3,-2,2,4,6,13)
    
    
    
    val cp = CPSolver()

    val a = CPVarInt(cp,dom)
    val b = CPVarInt(cp,dom)
    val c = CPVarInt(cp,dom)
    val d = CPVarInt(cp,dom)
    val e = CPVarInt(cp,dom)
    val f = CPVarInt(cp,dom)
    val g = CPVarInt(cp,dom)
    val h = CPVarInt(cp,dom)
    val s1 = CPVarInt(cp,(0 to (dom.max*4)^2).toSet)
    val s2 = CPVarInt(cp,(0 to (dom.max*4)^2).toSet)
    val obj = (s1*s1) + (s2*s2)

    cp.minimize(obj) subjectTo {
      
      cp.add(alldifferent(Array(a,b,c,d,e,f,g,h)), Strong)
      cp.add(sum(Array(a,b,c,d),s1))
      cp.add(sum(Array(e,f,g,h),s2))
      // break symmetries inside first set
      cp.add(a<b)
      cp.add(b<c)
      cp.add(c<d)
      // break symmetries inside second set
      cp.add(e<f)
      cp.add(f<g)
      cp.add(g<h)
      // break symmetries between the two sets
      cp.add(a<e)
      
      //not enough filtering the lower bound on s1 and s2 is 0 at the root node
      println("initial obj:"+obj)
      println("inits1:"+s1)
      println("inits2:"+s2)
      println("(a:"+a+" b:"+b+" c:"+c+" d:"+d+") (e:"+e+" f:"+f+" g:"+g+" h:"+h+")")
      println("---------------")
      
    } exploration {
      cp.binaryFirstFail(Array(a,b,c,d,e,f,g,h))
      println("(a:"+a+" b:"+b+" c:"+c+" d:"+d+") (e:"+e+" f:"+f+" g:"+g+" h:"+h+")")
    }

    cp.printStats()


  }

}

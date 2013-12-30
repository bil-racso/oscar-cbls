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



import scala.math

/**
 * Martin Garner Problem:
 * Let a,b,c,d,e,f,g and h be distinct elements in the set {-7,-5,-3,-2,2,4,6,13}
 * What is the minimum possible value of (a+b+c+d)^2 + (e+f+g+h)^2
 * @author Pierre Schaus pschaus@gmail.com
 */
object GardnerSumSquare extends App {
    val n = 5
    
    val dom = Set(-7,-5,-3,-2,2,4,6,13)
    
    
    
    val cp = CPSolver()

    val a = CPVarInt(dom)(cp)
    val b = CPVarInt(dom)(cp)
    val c = CPVarInt(dom)(cp)
    val d = CPVarInt(dom)(cp)
    val e = CPVarInt(dom)(cp)
    val f = CPVarInt(dom)(cp)
    val g = CPVarInt(dom)(cp)
    val h = CPVarInt(dom)(cp)
    val s1 = CPVarInt((0 to (dom.max*4)^2).toSet)(cp)
    val s2 = CPVarInt((0 to (dom.max*4)^2).toSet)(cp)
    val obj = (s1*s1) + (s2*s2)

    cp.minimize(obj) subjectTo {
      
      cp.add(allDifferent(Array(a,b,c,d,e,f,g,h)), Strong)
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
      
    } search {
      binaryFirstFail(Seq(a,b,c,d,e,f,g,h))
    } onSolution {
      println("(a:"+a+" b:"+b+" c:"+c+" d:"+d+") (e:"+e+" f:"+f+" g:"+g+" h:"+h+")")
    }
    println(cp.start())


  

}

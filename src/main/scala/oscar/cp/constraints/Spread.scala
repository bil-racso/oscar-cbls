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

package oscar.cp.constraints

import oscar.cp.core._
import oscar.cp.core.Constraint
import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome


/**
 * Implementation Spread Constraint
 * A Spread Constraint is the conjunction of two constraints
 * 1) Sum(i) x(i) == S (a constant)
 * 2) Sum(i) x(i)*x(i) = S2 (a variable)
 * And you generally want to maximize to minimize S2 knowing the sum
 * @author Pierre Schaus pschaus@gmail.com
 */
class Spread(val x: Array[CPVarInt], val S: Int, val S2: CPVarInt, val reverse: Boolean = true) extends Constraint(x(0).s, "Spread") {
  val n = x.size
  val xmin = Array.fill(n)(0)
  val xmax = Array.fill(n)(0)
  val bnds = Array.fill(2*n)((0,0)) // will contain the lower and upper bounds of the n domains + bounds type (1 = lower, 0 = upper)
 
  override def setup(l: CPPropagStrength): CPOutcome = { 
    
    if (reverse) {
      
      if (s.post(new Spread(x.map(-_),-S,S2,false)) == CPOutcome.Failure) {
    	  return CPOutcome.Failure
      } 
      
      if (decomposition() == CPOutcome.Failure) {
         return CPOutcome.Failure
      }
    }
    
    x.foreach(_.callPropagateWhenBoundsChange(this))
    S2.callPropagateWhenBoundsChange(this)
    propagate()
    
    CPOutcome.Suspend
  }
  
  def decomposition(): CPOutcome = {
    if (s.post(new Sum(x.map(i => i*i),S2)) == CPOutcome.Failure) {
      CPOutcome.Failure
    }
    else if (s.post(new Sum(x,S)) == CPOutcome.Failure) {
      CPOutcome.Failure 
    } else {
      CPOutcome.Suspend 
    }
    
  }

 
  override def propagate(): CPOutcome = {

	var summin = x.map(_.min).sum
	var summax = x.map(_.max).sum

	// check that sum constraint is feasible and prune it (fix point on it)
	for (i <- 0 until n) {
		//println("setting min to:"+(S - (summax-xmax(i)))+" summax:"+summax)
		if (x(i).updateMin(S - (summax - x(i).max)) == CPOutcome.Failure) {
		  //println("fail 1")
		  return CPOutcome.Failure
		}	
		if (x(i).updateMax(S - (summin - x(i).min)) == CPOutcome.Failure) {
		  //println("fail 2")
		  return CPOutcome.Failure
		}
	}
    if (x.forall(_.isBound)) {
      return S2.assign(x.map(i => i.value*i.value).sum)
    }
	for (i <- 0 until n) {
	  xmin(i) = x(i).min
	  xmax(i) = x(i).max
	}

	for(i <- 0 until n) {
	  bnds(i) = (x(i).min, 1)
	  bnds(n+i) = (x(i).max, 0)
	}
	def bound(i: Int) = bnds(i)._1
	def isLower(i: Int) = bnds(i)._2
	def isUpper(i: Int) = 1 - bnds(i)._2
	
	//sort the bounds
	scala.util.Sorting.quickSort(bnds)(Ordering.by[(Int, Int), Int](_._1))

	//compute |I| in O(n)
	val Isize = (1 until 2*n).count(i => bound(i) != bound(i-1))
	val bndSize = Isize + 1;
	
	//compute bounds counters
	val bndCptMin = Array.fill(bndSize)(0) //for each bound value, number of times it is a lower bound
	val bndCptMax = Array.fill(bndSize)(0) //for each bound value, number of times it is an upper bound
	val bounds = Array.fill(bndSize)(0)
	
	var k = 0
    bndCptMin(0) += isLower(0)
	bndCptMax(0) += isUpper(0)
	bounds(0) = bound(0)
	for(i <- 1 until 2*n){
	  if (bound(i) != bound(i-1)) {
	    k += 1
	  }
	  bounds(k) = bound(i)
	  bndCptMin(k) +=  isLower(i) // +1 iff lower bound at i
	  bndCptMax(k) += isUpper(i)  // +1 iff upper bound at i 
	}

	//compute l(I), r(I) and m(I) = n - r(I) - l(I) for every I
	val Imin = Array.tabulate(Isize)(i => bounds(i))
	val Imax = Array.tabulate(Isize)(i => bounds(i+1))
	
	
	val l = Array.fill(Isize)(0) 
	val r = Array.fill(Isize)(0)
	val m = Array.fill(Isize)(0)
	
	var lc = bndCptMax(0) // left count
	var rc = n-bndCptMin(0) // right count
	
	for (i <- 1 until bndSize) { 
	  l(i-1) = lc
	  r(i-1) = rc
	  m(i-1) = n-lc-rc
	  lc += bndCptMax(i)
	  rc -= bndCptMin(i)
	}
		
	//compute ES and ES2 in O(n)
	val ES = Array.fill(Isize)(0)
	val ES2 = Array.fill(Isize)(0)
	// compute ES(0) and ES2(0) with a sweep like algo

	for (i <- 0 until n) {	    
	    val xm = if (xmin(i) >= Imax(0)) xmin(i) else 0
		val xM = if (xmax(i) <= Imin(0)) xmax(i) else 0
		ES(0) += xm + xM
		ES2(0) += xm*xm + xM*xM
	}
	for (k <- 1 until Isize) {
		val pk = l(k) - l(k-1)
		val qk = r(k-1) - r(k)
		ES(k) = ES(k-1) + (pk-qk) * Imax(k-1)
		ES2(k) = ES2(k-1) + (pk-qk) * Imax(k-1) * Imax(k-1)
	}

	var Iopt = -1
	for (i <- 0 until Isize) {
		val minSi=ES(i)+m(i)*Imin(i)
		val maxSi=ES(i)+m(i)*Imax(i)
		if (S >= minSi && S <= maxSi) {
			Iopt = i
		} 
	}
	
	//for (i <- 0 until Isize) {
	  //println("I:"+Imin(i)+"-"+Imax(i)+" m:"+m(i)+" l:"+l(i)+" r:"+r(i)+" ES:"+ ES(i)+" ES2:"+ES2(i))
	//}
	
	//compute minimal spread in O(n)
	var minopt = 
	   if (m(Iopt) == 0) {
		   ES2(Iopt)
	   } else{
		   val vinf:  Int = ((S - ES(Iopt)) - ((S - ES(Iopt)) % m(Iopt))) / m(Iopt)
		   val vsup = vinf + (if ((S - ES(Iopt)) % m(Iopt) > 0) 1 else 0)
		   val y = (S - ES(Iopt)) % m(Iopt)
		   (ES2(Iopt) + y * (vsup * vsup) + (m(Iopt) - y) * (vinf * vinf))
	   }
	if (S2.updateMin(minopt) == CPOutcome.Failure) {
	  return CPOutcome.Failure 
	}
	val v: Double = (S - ES(Iopt)) / m(Iopt)
		
	// ----- filtering of x(i)'s max ------
	
	// return true if and only if Dom(x(i)) subsumes I
	def inDomain(I: Int, i: Int): Boolean = xmin(i) <= Imin(I) && xmax(i) >= Imax(I)
		
	def updateValues(i: Int, xi: Double, I: Int): (Double,Int,Double,Double) = {
	    val d: Double = xi - xmin(i)
	    val mStar: Int = if (inDomain(I,i)) m(I)-1 else m(I)
	    val esStar: Double = if (inDomain(I,i)) ES(I)+xi else ES(I)+d;
	    val es2Star: Double = if (inDomain(I,i)) ES2(I)+xi*xi else ES2(I)+d*d+2*d*xmin(i)
	    (d,mStar,esStar,es2Star)
	}
	
    def pruneMax(i: Int, xiq: Double, I: Int): CPOutcome = {
	    if (I >= 0 && xiq < xmax(i)) {
	      val (d,mStar,esStar,es2Star) = updateValues(i,xiq,I)
	      val IS_ = esStar + mStar * Imin(I)
	      if (mStar > 0) {    
	    	  	val vStar = (S - esStar) / mStar
                val deltaQ = (es2Star + mStar * vStar * vStar)
                val d1 = S - IS_
                val a= (1.0 + 1.0 / mStar)
                val b= (xiq - vStar)
                val c = deltaQ - S2.max
                val d2 = (-b + scala.math.sqrt(b*b-a*c))/a
                if(d2 <= d1) {
                  val xiz = scala.math.floor(xiq+d2).toInt
                  x(i).updateMax(xiz)
                } 
                else pruneMax(i,xiq+ d1, I-1)	        
	      } else pruneMax(i,xiq, I-1)
	    } else CPOutcome.Suspend
	}

	
	for (i <- 0 until n; if xmax(i) > Imin(Iopt)) {
	  if (pruneMax(i,if (inDomain(Iopt,i)) v else xmin(i), Iopt) == CPOutcome.Failure) {
	    return CPOutcome.Failure
	  }
	}
    return CPOutcome.Suspend
  }

}



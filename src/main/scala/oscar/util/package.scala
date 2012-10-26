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

package oscar


package object util {
  val rand = new scala.util.Random(0)
  
  /**
   * Random selector:
   * @return some randomly selected value i in r satisfying st(i)
   * @author pschaus
   */
  def select[R](r: Iterable[R])(st: (R => Boolean) = ((r: R) => true)): Option[R] = {      
    var cpt = 1
    var result: Option[R] = None 
    for (o <- r; if st(o)) {
      val proba = 1.0/cpt
      if (rand.nextDouble() <= proba) {
        result = Some(o)
      }
      cpt += 1
    }
    result
  }

  
  /**
   * Random min selector
   * @return some randomly selected value i in r, minimizing f(i) and satisfying st(i)
   * @author pschaus
   */
  def selectMin[R](r: Iterable[R])(st: (R => Boolean) = ((r: R) => true))(f: R => Double): Option[R] = {
    var cpt = 1
    var result: Option[R] = None
    var best = Double.MaxValue
    for (o <- r; if st(o)) {
      val eval = f(o)
      if (eval < best) {
        result = Some(o)
        cpt = 2
        best = eval
      } else if (eval == best) {
        val proba = 1.0/cpt
        if (rand.nextDouble() <= proba) {
          result = Some(o)
        }
        cpt += 1
      }
    }
    result
  }
  
  /**
   * Random max selector
   * @author pschaus
   */
  def selectMax[R](r: Iterable[R])(st: (R => Boolean) = ((r: R) => true))(f: R => Double) = selectMin(r)(st)(-f(_))
    
  

}
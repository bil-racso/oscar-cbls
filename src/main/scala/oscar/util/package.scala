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
  def selectMin[R, T](r: Iterable[R])(st: (R => Boolean) = ((r: R) => true))(f: R => T)(implicit orderer: T => Ordered[T]): Option[R] = {
    r.find(st) match {
      case Some(v) => {
        var cpt = 1
        var result = v
        var best: T = f(v)
        for (o <- r; if st(o)) {
          val eval: T = f(o)
          if (orderer(eval) < best) {
            result = o
            cpt = 2
            best = eval
          } else if (eval == best) {
            val proba = 1.0 / cpt
            if (rand.nextDouble() <= proba) {
              result = o
            }
            cpt += 1
          }
        }
        Some(result)
      }
      case None => None
    }
  }
  
  
  /**
   * Random min selector
   * @return the k value i in r minimizing f(i) and satisfying st(i). In case of tie, those are broken randomly.
   * @author pschaus
   */
  def selectMinK[R, T](r: Seq[R],k: Int)(st: (R => Boolean) = ((r: R) => true))(f: R => T)(implicit orderer: T => Ordered[T]): Iterable[R] = {
    val potentials = r.filter(st(_))
    if (potentials.size <= k) potentials
    else {
      val grouped = potentials.groupBy(x => f(x)).toArray
      val groupedSorted = grouped.sortWith{(x1,x2) =>  orderer(x1._1) < x2._1}
      val sorted = groupedSorted.map(_._2).flatMap(rand.shuffle(_))
      sorted.take(k)
    }
  }
  
 
  /**
   * @param block a code block
   * @return the time (ms) to execute the block
   */
  def time(block: => Unit): Long = {
    val t0 = System.currentTimeMillis();
    block
    System.currentTimeMillis - t0;
  }

}
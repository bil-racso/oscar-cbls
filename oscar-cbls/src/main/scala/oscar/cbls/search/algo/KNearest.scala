/**
 * *****************************************************************************
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
 * ****************************************************************************
 */

package oscar.cbls.search.algo

import oscar.cbls.invariants.core.algo.heap.BinomialHeap

/**
 * This class serves to compute the k-smallest values of a given vector.
 * this computation can be done either one-shot, or with gradually increasing k
 */
object KSmallest {

  def apply(a:Array[Int],k:Int):List[Int] = {
    val heap = new BinomialHeap[Int](indice => -a(indice),k+1)
    for(i <- a.indices){
      heap.insert(i)
      if(i > k)heap.popFirst()
    }
    heap.toList
  }

  def apply(a:Array[Int]):KSmallest = new KSmallest(a)
}

class KSmallest(a:Array[Int]){
  //zipWithIndex puts index in second position of the couple
  val sorted:List[Int] = a.toList.zipWithIndex.sortBy(_._1).map(_._2)

  def apply(k:Int):List[Int] = sorted.take(k)

  def apply(k:Int,filter:Int=>Boolean) = {
    def kSmallestAcc(sorted: List[Int], k: Int): List[Int] = {
      require(k >= 0)
      if(k == 0) return Nil
      sorted match {
        case Nil => Nil
        case h :: t =>
          if (filter(h)) h :: kSmallestAcc(t, k - 1)
          else kSmallestAcc(t, k)
      }
    }
    kSmallestAcc(sorted, k)
  }
}

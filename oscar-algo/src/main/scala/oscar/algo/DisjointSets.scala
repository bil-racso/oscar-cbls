package oscar.algo

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

import scala.annotation.tailrec


class DisjointSets(min: Int, max: Int) {
  
  val all = Array.tabulate(max-min+1)(i => makeSet(min+i))
  
  def reset() {
    var i = 0
    while (i < all.size) {
      all(i).reset
      i += 1
    }
  }
  
  def union(v1: Int, v2: Int) {
    union(all(v1-min),all(v2-min))
  }
  
  def inSameSet(v1: Int,v2: Int) = {
    findSet(all(v1-min)) == findSet(all(v2-min))
  }
  
  def find(v: Int) = {
    findSet(all(v-min))
  }
  
  class Set(private val elem : Int) {
	var max = elem
	var min = elem
    var rank : Int = 0
    var parent : Set = this
    
    def reset() {
	  max = elem
	  min = elem
	  rank = 0
	  parent = this
	}
    
  }
  
  def union(x: Set, y: Set) {
    link(findSet(x),findSet(y))
  }
  
  private def link(x: Set, y: Set) {
    if (x == y) return
    if (x.rank > y.rank) {
      y.parent = x
      // x becomes root
      x.max = x.max max (y.max)
      x.min = x.min min (y.min)
    } else {
      // y becomes root
      y.max = y.max max (x.max)
      y.min = y.min min (x.min)
      x.parent = y
      if (x.rank == y.rank) {
        y.rank += 1
      }
    }
  }
  
  
  private def makeSet(x: Int) = {
    new Set(x)
  }
  
  private def findSet(x: Set): Set = {
    if (x != x.parent) {
      x.parent = findSet(x.parent)
    }
    x.parent
  }
  
}



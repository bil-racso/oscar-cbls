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
package oscar.algo.test


import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import oscar.algo.search._
import oscar.algo.reversible._
import oscar.algo.DisjointSets
import oscar.algo.DisjointSets
import oscar.algo.CCTree
import oscar.algo.CCTreeNode
import oscar.algo.RangeMinQuery


class DisjointSetsTest extends FunSuite with ShouldMatchers  {

  test("test ds1") {
    val sets = new DisjointSets(1,4)
    sets.find(1).min should be(1)

    sets.union(1,2)
    sets.inSameSet(1,2) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(1).min should be(1)
    sets.find(1).max should be(2)
    
    
    sets.union(3,4)
    sets.inSameSet(3,4) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(4).min should be(3)
    sets.find(3).max should be(4)
    
    sets.union(1,1)
    sets.union(1,2)
    sets.inSameSet(1,2) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(1).min should be(1)
    sets.find(1).max should be(2)
    sets.inSameSet(3,4) should be(true)
    sets.inSameSet(1,4) should be(false)
    sets.find(4).min should be(3)
    sets.find(3).max should be(4)
    
    
    sets.union(2,3)
    sets.inSameSet(1,4) should be(true)
    sets.find(3).min should be(1)
    sets.find(1).max should be(4)
    sets.inSameSet(3,4) should be(true)
    sets.inSameSet(2,4) should be(true)
    sets.find(4).min should be(1)
    sets.find(3).max should be(4)   
  }
  
  test("test ds2") {
    
    val sets = new DisjointSets[CCTreeNode](0,5)
    val cctree = new CCTree(6)
    sets.resetAndSetData(i => cctree.nodes(i))
    
    val t0 = sets.find(0).data.get
    val t1 = sets.find(1).data.get
    val t6 = cctree.merge(t0,t1,6)
    sets.union(0,1,t6)
    
    val t2 = sets.find(2).data.get
    val t3 = sets.find(3).data.get
    val t7 = cctree.merge(t2,t3,7)
    sets.union(2,3,t7)    
    
    val t4 = sets.find(4).data.get
    val t5 = sets.find(5).data.get
    val t8 = cctree.merge(t4,t5,8)
    sets.union(4,5,t8)       
 
    sets.find(0).data.get should be(t6)
    sets.find(2).data.get should be(t7)
    val t9 = cctree.merge(t6,t7,9)
    sets.union(0,2,t9)
    
    sets.find(0).data.get should be(t9)
    sets.find(4).data.get should be(t8)
    val t10 = cctree.merge(t9,t8,10)
    sets.union(0,2,t10)
    
    val inorder = cctree.inorderCollect()
    
    inorder.map(_.value).filter(_ > 0) should be(Array(6,9,7,10,8))
    
    println("inorder:"+inorder.map(_.value).mkString(","))
    println("inorder heights:"+inorder.map(_.height).mkString(","))
    
    val pos = Array.fill(11)(0)
    for (i <- 0 until 10) {
      pos(inorder(i).index) = i
    }
    val heights = inorder.map(_.height)
    val rmq = new RangeMinQuery(heights)
    //println("lca of 1,3 should be 9:"+inorder(rmq(pos(1),pos(3))).value)
    inorder(rmq(pos(1),pos(3))).value should be(9)
    inorder(rmq(pos(0),pos(4))).value should be(10)
    inorder(rmq(pos(2),pos(3))).value should be(7)
    inorder(rmq(pos(2),pos(4))).value should be(10)
    
    
    
    
    
  }


}


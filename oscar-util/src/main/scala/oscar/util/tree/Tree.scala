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
package oscar.util.tree

import oscar.util.tree._
import java.awt.Color

/**
 * search tree encoding for visualization
 * @author Pierre Schaus pschaus@gmail.com
 */
class Tree(var record: Boolean = true) {
  var branches: List[(Int,Int,String,String,() => Unit)] = Nil
  
  var succ: List[Int] = Nil
  
  def addBranch(parentId: Int, id: Int, nodeName: String="", branchName: String="") {
	createBranch(parentId,id,nodeName,branchName){}
  }
  
  def createBranch(parentId: Int, id: Int, nodeName: String="", branchName: String="")(action: => Unit) {
	//<try id="1" parent="0" name="a" size="2" value="1"/>
    if (record) branches = (parentId,id,nodeName,branchName,() => action) :: branches
  }
  
  def addSuccess(id: Int) {
    if (record) succ = id :: succ
  }
  
  def clear() = {
    branches = Nil
    succ = Nil
  }
  
  /**
   * returns branches with n as parent
   */
  def children(n: Int) = {
    branches.filter(b => b._1 == n)
  }
  
  private def action(n: Int):() => Unit = {
    branches.find{case(parentId,id,nodeName,branchName,action) => id == n}.map(_._5) match {
      case Some(action) => action
      case None => () => Unit
    }
  }
  
  def toNode(n: Int = 0): Node[String] = {
    //println("succ:"+succ)
    val childs = children(n).reverse
    if (childs.isEmpty) {
      Node(n.toString,if(succ.contains(n)) Color.green else Color.white,action(n))
    } else {
      Node(n.toString,childs.map(c => toNode(c._2)),childs.map(_._4),if(succ.contains(n)) Color.green else Color.white,action(n))
    }
  }
  
  
  def toXml() = {
    <tree version="1.0">
		  <root id="0"/>
	  	  {branches.map{case(parent,id,name,value,action) => 
	  	                   		<try id= {id.toString} parent = {parent.toString} name= {name} value = {value} />
	  	                   }
	  	  
	  	  
	  	  }
	  	  {succ.map{i => 
	  	              <succ id={i.toString} /> }
	  	  }
    </tree>
  }
  
  def save(file: String) {
    scala.xml.XML.save(file,toXml())
  }
  
  
}

object Tree {
  def main(args: Array[String]) {
	  val t = new Tree(true)
	  t.addBranch(0, 1)
	  t.addBranch(0,4)
	  //println(t.children(0))
	  //t.toNode(0)
	  println(t.toNode(0))
  }
}

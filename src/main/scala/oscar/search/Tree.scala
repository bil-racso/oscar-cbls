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

package oscar.search

/**
 * search tree encoding for visualization
 * @author Pierre Schaus pschaus@gmail.com
 */
class Tree(var record: Boolean = true) {
  var branches: List[(Int,Int,String,String)] = Nil
  
  var succ: List[Int] = Nil
  
  def addBranch(parentId: Int, id: Int, name: String, value: String) {
	//<try id="1" parent="0" name="a" size="2" value="1"/>
    if (record) branches = (parentId,id,name,value) :: branches
  }
  
  def addSuccess(id: Int) {
    if (record) succ = id :: succ
  }
  
  def clear() = {
    branches = Nil
    succ = Nil
  }
  
  
  def toXml() = {
    <tree version="1.0">
		  <root id="0"/>
	  	  {branches.map{case(parent,id,name,value) => 
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
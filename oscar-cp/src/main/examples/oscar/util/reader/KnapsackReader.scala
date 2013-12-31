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
package oscar.examples.cp.util.reader

import scala.io.Source

object KnapsackReader {
  
  def read(dataFile: String) = {
    
    // Read the data
    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")
    val n: Int = lines.head.toInt
    val N = 0 until n
    lines = lines.drop(1)
    val capa = lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray[Int]
    val capa1 = capa(0)
    val capa2 = capa(1)
    lines = lines.drop(1)

    
    val itemsKnapsack1: Array[(Int,Int)] = 
    (for (i <- N) yield {
     val l = lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
     lines = lines.drop(1)
     (l(0),l(1)) // weight, profit
    }).toArray
    
    val itemsKnapsack2: Array[(Int,Int)] = 
    (for (i <- N) yield {
     val l = lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
     lines = lines.drop(1)
     (l(0),l(1))
    }).toArray
    (n,capa1,capa2,itemsKnapsack1,itemsKnapsack2)
  }
  
  def readSolution(solFile: String): Array[(Int,Int)] = {
    
    // Read the data
    var lines = Source.fromFile(solFile).getLines.toList.filter(_ != "")
    (for (l <- lines) yield {
     val s = l.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
     (s(0),s(1))
    }).toArray
  }

 

}

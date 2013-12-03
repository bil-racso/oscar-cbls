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
import scala.Array.canBuildFrom

object QAPReader {
  
  
  def read(dataFile: String, n: Int) = {
    val N = 0 until n
    // Read the data
    var lines = Source.fromFile(dataFile).getLines.toList.filter(_ != "")
    lines = lines.drop(1)
    var w1: Array[Array[Int]] = Array() //weight matrix 1
    var w2: Array[Array[Int]] = Array() //weight matrix 2
    var d: Array[Array[Int]] = Array() //distance matrix
    for (i <- N) {
      d = d :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    for (i <- N) {
      w1 = w1 :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    for (i <- N) {
      w2 = w2 :+ lines.head.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      lines = lines.drop(1)
    }
    (d,w1,w2)
  }

  def readSolutions(solFile: String, n: Int) = {
    var lines = Source.fromFile(solFile).getLines.toList.filter(_ != "")
    for (l <- lines) yield {
      val ar = l.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      (ar.take(n), ar.drop(n))
    }
  }
  
  def readSolutions2(solFile: String, n: Int) = {
    var lines = Source.fromFile(solFile).getLines.toList.filter(_ != "")
    for (l <- lines) yield {
      val ar = l.split("[ ,\t]+").filter(_ != "").map(_ toInt).toArray
      (ar.drop(2), ar.take(2))
    }
  }

}

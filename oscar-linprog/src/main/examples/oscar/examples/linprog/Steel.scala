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
package oscar.examples.linprog

import oscar.linprog.modeling._
import oscar.algebra._
import scala.io.Source

/**
 *
 * Model for the steel mill slab problem:
 * Steel is produced by casting molten iron into slabs.
 * A steel mill can produce a finite number, σ, of slab sizes.
 * An order has two properties, a color corresponding to the route required through the steel mill and a weight.
 * Given d input orders, the problem is to assign the orders to slabs,
 * the number and size of which are also to be determined,
 * such that the total weight of steel produced is minimized.
 * This assignment is subject to two further constraints:
 *    - Capacity constraints: The total weight of orders assigned to a slab cannot exceed the slab capacity.
 *    - Color constraints: Each slab can contain at most p of k total colors (p is usually 2).
 * See problem 38 of http://www.csplib.org/ or http://becool.info.ucl.ac.be/steelmillslab
 * @author Pierre Schaus pschaus@gmail.com
 */
object Steel {

  class Column(val x: LPFloatVar, val capa: Int, val contains: Array[Int]) {
    override def toString(): String = {
      contains.mkString("\t")
    }
    def number(): Int = Math.ceil(x.value.get).toInt
  }

  def readData(): (Array[Int], Array[Int], Array[Int]) = {
    val lines = Source.fromFile("data/steelMillSlabEasy.txt").getLines.reduceLeft(_ + " " + _)
    var vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
    val nbCapa = vals.head
    vals = vals.drop(1)
    var (capa, vals_) = vals splitAt nbCapa
    capa = 0 :: capa
    val maxcapa = capa.max
    val nbCol = vals_.head
    vals_ = vals_.drop(1)
    val nbSlab = vals_.head
    vals_ = vals_.drop(1)
    var weight = Array[Int]()
    var col = Array[Int]()
    for (i <- 1 to nbSlab) {
      vals_ match {
        case w :: c :: v =>
          vals_ = vals_.drop(2)
          weight = weight :+ w
          col = col :+ c - 1 //color starts at 1 in input file
        case Nil => Unit
      }
    }
    (capa toArray, weight, col)
  }

  def main(args: Array[String]): Unit = {

    val (capa, weight, col) = readData()
    val (nbCapa, nbSlab, nbCol) = (capa.length, weight.length, col.max + 1)
    val Slabs = 0 until nbSlab
    val Cols = 0 until nbCol
    val loss = (0 to capa.max).map(c => capa.filter(_ >= c).min - c)
    val colorOrders = Cols.map(c => (Slabs).filter(s => col(s) == c))

    implicit val lp = LPSolver(LPSolverLib.lp_solve)

    var C: Array[Column] = Array()

    val Capa = Array.tabulate(capa(capa.size - 1))(_ => 0)

    for (s <- 0 until nbSlab) {
      val cont = Array.tabulate(nbSlab)(_ => 0)
      cont(s) = 1
      C = C :+ new Column(LPFloatVar("x", 0, 10000), loss(weight(s)), cont)
    }

    var meet: Array[LPConstraint] = Array()

    // Master Problem 
    minimize(sum(C)(c => c.x * c.capa))
    for (s <- 0 until nbSlab) {
      meet = meet :+ add(sum(C)(c => c.x * c.contains(s)) == 1)
    }
    start()

    // Pricing Problem
    var added = false
    for (CAPA <- capa) {
      do {
        added = false
        val mip = MIPSolver(LPSolverLib.lp_solve)

        val use = Array.tabulate(nbSlab)(_ => MIPIntVar(mip, "use", 0 to 1))
        val v = Array.tabulate(nbCol)(_ => MIPIntVar(mip, "v", 0 to 1))
        val cost = Array.tabulate(nbSlab)(meet(_).dual)

        mip.maximize(sum(Slabs)(s => ((cost(s) + weight(s)) * use(s))))

        mip.add(sum(Slabs)(s => use(s) * weight(s)) <= CAPA)
          for (s <- Slabs)
            mip.add(use(s) <= v(col(s)))
          mip.add(sum(Cols)(c => v(c)) <= 2)
        mip.start()

        if (mip.objectiveValue.get > CAPA + 0.01) {
          var a = new Array[scala.Double](nbSlab)
          var tmp = 0
          for (i <- 0 until nbSlab) {
            a(i) = use(i).value.get.toInt * loss(weight(i))
            tmp += (use(i).value.get.toInt * weight(i))
          }

          val x = lp.addColumn(loss(tmp), meet, use.map(_.value.get)) //create a new variable by introducing a new column
          C = C :+ new Column(x, loss(tmp), use.map(_.value.get.toInt))
          added = true
        }

        println("==> master obj:" + lp.objectiveValue)

      } while (added)
    }
    println("objective: " + lp.objectiveValue)
    println("nbColumns: " + C.size)
  } // end of main
}

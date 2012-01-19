/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package scampi.cp.examples


import scampi.cp.modeling._
import scampi.cp.search._
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Random
import java.lang._
import scampi.search.Branching

/**
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
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object Steel extends CPModel{


  def readData(): (Array[Int], Array[Int], Array[Int]) = {
    val lines = Source.fromFile("data/steelMillSlab.txt").getLines.reduceLeft(_ + " " + _)
    var vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
    val nbCapa = vals.head
    vals = vals.drop(1)
    var (capa, vals_) = vals splitAt nbCapa
    capa = 0 :: capa
    val maxcapa = capa.max
    println(capa, vals_)
    val nbCol = vals_.head
    vals_ = vals_.drop(1)
    val nbSlab = vals_.head
    vals_ = vals_.drop(1)
    var weight = Array[Int]()
    var col = Array[Int]()
    for (i <- 1 to nbSlab) {
      vals_ match {
        case w :: c :: v => vals_ = vals_.drop(2)
        weight = weight :+ w
        col = col :+ c - 1 //color starts at 1 in input file
        case Nil => Unit
      }
    }
    (capa toArray, weight, col)
  }



  def main(args: Array[String]) {
		val (capa,weight,col) = readData()
		val (nbCapa,nbSlab,nbCol) = (capa.length, weight.length, col.max+1)
		val Slabs = 0 until nbSlab
		val Cols = 0 until nbCol
		val loss = (0 to capa.max).map(c => capa.filter(_ >= c).min - c)
		val colorOrders = Cols.map(c => (Slabs).filter (s => col(s)==c))

		val cp = new CPSolver
		val x = (for(s <- Slabs) yield CPVarInt(cp,0 until nbSlab))
		val l = for(s <- Slabs) yield CPVarInt(cp,0 to capa.max)
		val xsol = (for(s <- Slabs) yield 0) toArray //current best solution

		cp.onSolution { //record the current best solution to be used by the LNS
			Slabs.foreach( s => xsol(s) = x(s).getValue)			
		}

		val rnd = new Random(0)
		cp.lns(200,50) {
		  for (s <- Slabs; if rnd.nextInt(100) > 80) {
		    cp.add(x(s) == xsol(s))
		  }
		}	

		cp.minimize(sum(Cols)(s => element(loss,l(s)))) subjectTo {
			cp.add(binpacking(x,weight,l),Strong)
			for (s <- Slabs) {
				def colPresent(c : Int) = or ((for (o <- colorOrders(c)) yield x(o) === s) toArray) //return a CPVarBool telling whether color c is present is slab s
				cp.add(sum(Cols)(c => colPresent(c)) <= 2) //at most two colors present in each slab
			}
		} exploring {
			val currLoads = Array.tabulate(nbSlab)(s => 0)
			for (s <- Slabs; if (x(s).isBound())) {
			  currLoads(x(s).getValue) += weight(s)
			}
			def lossDelta(o: Int, s : Int) = loss(currLoads(s) + weight(o)) - loss(currLoads(s)) //delta on the loss if you place order o in slab s
			val res = minDomNotbound(x)

			if (res.size > 0) { //not every variables are bound			
			    val (y,i) = res(0)
				val bound = x.filter(_.isBound())
				val maxboundval = if (bound.size == 0) 0 else (bound.map(_.getValue()).max + 1)
				cp.branchOn(y.getMin() to maxboundval, //range of values
					        s => y.hasValue(s) && currLoads(s)+weight(i) <= capa.max, //we only try values in the domain
					        s => lossDelta(i,s)) { //try first value that will induce the smallest increase of loss
						  	   v => y == v
					        }
			}
			else {
				Branching.noAlternative 
			}
		}

		cp.printStats()

	}
}
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
import scampi.search._
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.Random
import java.lang._
import scampi.visual._
import java.awt.Color


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
		val xsol = Array.fill(nbSlab)(0) //current best solution
		
		
		// -------------visual components ------------
		val colors = VisualUtil.getRandomColorArray(nbCol)
		val scale = 7
		val f = new VisualFrame("Steel Mill Slab")
		// creates the plot and place it into the frame
		val plot = new Plot2D("","Solution number","Loss")
		f.createFrame("Objective").add(plot)
		// creates the tour visu and place it into the frame
		val drawing: VisualBinPacking = new VisualBinPacking(nbSlab,12)
		val items = Slabs.map(i => drawing.addItem(i,scale*weight(i))).toArray
		Slabs.foreach(o => items(o).setInnerCol(colors(col(o))))
		f.createFrame("Steel Mill Slab").add(drawing)
		capa.foreach(c => new VisualLine(drawing,0,c*scale,nbSlab*12,c*scale).setOuterCol(Color.red))
		f.pack()
		// ------------------------------------------


		val rnd = new Random(0)
		var nbSol = 0
		cp.lns(100,200) {
		  for (s <- Slabs; if rnd.nextInt(100) > 70) {
		    cp.post(x(s) == xsol(s))
		  }
		}
		val obj = sum(Slabs)(s => element(loss,l(s)))
		cp.minimize(obj) subjectTo {
			cp.add(binpacking(x,weight,l),Strong)
			for (s <- Slabs) {
				def colPresent(c : Int) = or ((for (o <- colorOrders(c)) yield x(o) === s) toArray) //return a CPVarBool telling whether color c is present is slab s
				cp.add(sum(Cols)(c => colPresent(c)) <= 2) //at most two colors present in each slab
			}
		} exploration {
		  while (!allBounds(x)) {
		    val bound = x.filter(_.isBound())
		    val maxUsedSlab = if (bound.isEmpty) -1 else bound.map(_.getValue()).max
		    //delta on the loss if you place order o in slab s
		    val (y,o) = minDomNotbound(x).first // retrieve the var and its index in x with smallest domain
		    val v = y.getMin()
		    if (v > maxUsedSlab) { // o can only be placed in an empty slab (=> dynamic break of symmetries)
		      cp.branchOne(cp.post(y == v))
		    }
		    else  {
		      cp.branch(cp.post(y == v))(cp.post(y != v))
		    }
		  }
		  
		  println("failed:"+cp.isFailed())
		  Slabs.foreach(o => {xsol(o) = x(o).getValue
		                     items(o).setBin(xsol(o))})
		  plot.addPoint(nbSol,obj.getValue())                   
		  nbSol += 1
		  println("sol #fail:"+cp.sc.nbFail)
		}
		println("end--------------")

		cp.printStats()

	}
}
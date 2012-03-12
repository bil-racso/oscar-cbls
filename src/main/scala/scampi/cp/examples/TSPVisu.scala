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
import scampi.visual._
import scala.collection.JavaConversions._
import scala.io.Source
import java.lang._
import java.awt.Color


/**
 * 
 * tsp model with visualization: 
 * given a distance matrix between 20 cities, 
 * find the shortest tour visiting each city exactly once.
 * 
 * @author Pierre Schaus pschaus@gmail.com
 */
object TSPVisual extends CPModel {

  def main(args: Array[String]) {
    
    // -------------visual components ------------
    val f = new VisualFrame("TSP")
    // creates the plot and place it into the frame
	val plot = new Plot2D("","Solution number","Distance")
    f.createFrame("TSP Objective Function").add(plot)
    // creates the tour visu and place it into the frame
    val drawing = new VisualDrawing(false)
    f.createFrame("TSP Tour").add(drawing)
    f.pack()
    // ------------------------------------------
    

    val n = 18
    val Cities = 0 until n

    val rand = new scala.util.Random(0)
    val coord = Array.tabulate(n)(i => (100+rand.nextInt(400),rand.nextInt(400)))
    
    
    val lines = Array.tabulate(n)(i => new VisualLine(drawing,coord(i)._1,coord(i)._2,0,0))

    
    def getDist(p1: (Int,Int), p2: (Int,Int)): Int = {
      val dx = p2._1 - p1._1
      val dy = p2._2 - p1._2
      math.sqrt(dx*dx + dy*dy).asInstanceOf[Int]
    }
    
    val distMatrix = Array.tabulate(n,n)((i,j) => getDist(coord(i),coord(j)))
    
    println(distMatrix)

    val cp = new CPSolver()
    //array of successors
    val succ = Array.tabulate(n)(_ => CPVarInt(cp, 0 until n))
    //total distance
    val dist = CPVarInt(cp, 0 to distMatrix.flatten.sum)
    
    
    // -----------------visual update -----------
    var nbSol = 0
    coord.foreach(c => new VisualCircle(drawing, c._1 , c._2, 2, Color.blue))
    def updateVisu() {
      def update(i: Int) = lines(i).setDest(coord(succ(i).getValue())._1,coord(succ(i).getValue())._2)
      nbSol += 1
      (0 until n).foreach(update(_))
      drawing.repaint()
      plot.addPoint(nbSol,dist.getValue())
    }
    // ------------------------------------------
    
    cp.minimize(dist) subjectTo {
      cp.add(circuit(succ), Strong) //ask to have a strong filtering
      cp.add(sum(Cities)(i => element(distMatrix(i), succ(i))) == dist)
    } exploration {
      //exploration of the search tree
      while (!allBounds(succ)) {
         val res = minDomNotbound(succ)
         val (x, i) = res.first
         // get the closest successor in the domain of x
         val v = argMin((x.getMin() to x.getMax()).filter(x.hasValue(_)))(distMatrix(i)(_)).first
         cp.branch(cp.post(x == v)) (cp.post(x != v))
      }
      updateVisu()
    }

    cp.printStats()
  }

}
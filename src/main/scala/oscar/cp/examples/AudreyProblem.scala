/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 *  
 * Contributors:
 *      www.n-side.com
 ******************************************************************************/
package oscar.cp.examples


import oscar.cp.modeling._
import oscar.search._
import oscar.visual._
import java.awt.Color

/**
 * Little Problem given by my n-Side colleague Audrey Timmermans:
 * Based on a little game I used to play in high school when I was getting bored in the classroom...
 * Draw a ten cells by ten cells board.
 * The purpose is to fill in all cells with numbers from 1 to 100.
 * You start by writing 1 in whatever cell. 
 * From there on, you need to write the 2 by moving around in one of the following ways:
 * - Move by 3 cells horizontally or vertically
 * - Or move by 2 cells diagonally
 * Then, starting from the 2, you need to write the 3 using the same permitted moves, and so on...
 * @author Pierre Schaus pschaus@gmail.com
 */
object AudreyProblem  extends CPModel {
	def main(args: Array[String]) {

		def reachables(i : Int) : Set[Int] = {
		    val l = i/10
		    val c = i%10 
		    val neighbors = Set((l-3,c),(l+3,c),(l,c+3),(l,c-3),(l+2,c+2),(l+2,c-2),(l-2,c+2),(l-2,c-2)).filter{ case (a,b) => a < 10 && a >= 0 && b < 10 && b >= 0}
		    neighbors.map{case(a,b) => a * 10 + b}
		}
		
		val cp = new CPSolver()
		val x = (0 until 100).map(v => CPVarInt(cp,reachables(v)))
		
		cp.solve subjectTo {
			cp.add(circuit(x))
		} exploration {
		  cp.binaryFirstFail(x)
		  println(x.map(_.getValue).mkString(","))
		}
		
		cp.printStats()
		
		//  -----------visualization of the euler tour ----------
	
		val f = new VisualFrame("Audrey",1,1)
		val drawing = new VisualDrawing(false)
		f.createFrame("Audrey").add(drawing)
		val scale = 60

		for (i <- 0 until 10; j <- 0 until 10) {
		  val rect = new VisualRectangle(drawing,i*scale,j*scale,scale,scale)
		  if (i % 2 == 0) {
		    if (j % 2 == 0) rect.setInnerCol(Color.gray)
		  } else {
		    if (j % 2 == 1) rect.setInnerCol(Color.gray)
		  }	
		}		
		for (i <- 0 until 100) {
		  val v = x(i).getValue()
		  val (c,l) = (v/10, v%10)
		  new VisualCircle(drawing,scale/2+(i/10)*scale,scale/2+(i%10)*scale,3).setInnerCol(Color.RED)
		  new VisualLine(drawing,scale/2+(i/10)*scale,scale/2+(i%10)*scale,scale/2+c*scale,scale/2+l*scale)
		}
		var curr = 0
		for (i <- 0 until 100) {
		  val l = curr/10
		  val c = curr%10
		  
		  new VisualText(drawing,scale/2+l*scale,scale/2+c*scale,i.toString)
		  curr = x(curr).getValue()
		}
		f.pack()
		drawing.repaint()

	}

}

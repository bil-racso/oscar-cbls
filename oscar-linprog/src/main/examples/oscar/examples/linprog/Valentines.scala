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
import oscar.visual.VisualFrame
import oscar.visual.VisualTable
import java.awt.Color

/**
 * Tallys Yunes described this bouquet delivery problem in his blog: 
 * http://orbythebeach.wordpress.com/2011/02/04/transporting-flowers-with-love/
 * @author Alastair Andrew alastair.andrew@gmail.com
 */
object Valentines extends LPModelLPSolve with App {
	
  // The Problem Data
  val Florists = 8 // No. of Flower Shops
  val Valentines = 9 // No. of Receivers
  
  val stock = Array(4, 4, 3, 2, 2, 2, 2, 1) // No. of Bouquets in each shop
  val requests = Array(3, 2, 2, 2, 2, 2, 2, 2, 3) // Min. No. of Bouquests each Valentine needs
  val cost = Array(Array(9, 1, 2, 4, 4, 3, 1, 2, 5), // The cost (in  $) of sending a bouquet from a shop    
		  	       Array(3, 2, 7, 3, 8, 1, 3, 5, 1), // to a valentine.
		  		   Array(1, 3, 5, 4, 2, 3, 5, 7, 2),
		  		   Array(2, 3, 4, 5, 6, 5, 4, 8, 3),
		  		   Array(6, 1, 5, 8, 7, 4, 9, 1, 6),
		  		   Array(7, 2, 3, 6, 5, 6, 2, 3, 8),
		  		   Array(4, 5, 6, 2, 4, 2, 7, 5, 4),
		  		   Array(4, 3, 4, 5, 3, 7, 6, 5, 9))
  
  // Setting up the GUI (to resemble Tallys' Excel Spreadsheet)
  val f = new VisualFrame("Transporting Flowers with Love")
  val table = new VisualTable(Florists + 3, Valentines + 3) 
  
  (0 until Florists).foreach(f => table.setRowName(s"F${f+1}", f))
  (0 until Valentines).foreach(v => table.setColumnName(s"V${v+1}", v))
  table.setColumnName("Outflows", Valentines)
  (1 to 2).foreach(v => table.setColumnName("", Valentines + v))
  table.setRowName("Inflows", Florists)
  (1 to 2).foreach(f => table.setRowName("", Florists + f))
  
  for(f <- 0 until Florists){
    table.setValueAt("0", f, Valentines)
    table.setValueAt("=", f, Valentines + 1)
    table.setValueAt(stock(f).toString, f, Valentines + 2)
  }
  
  for(v <- 0 until Valentines){
	  table.setValueAt("0", Florists, v)
	  table.setValueAt("=", Florists + 1, v)
	  table.setValueAt(requests(v).toString, Florists + 2, v)
  }
  
  table.setValueAt("Total Cost", Florists + 1, Valentines)
  table.setValueAt("0", Florists + 2, Valentines)
  
  (0 until Florists).foreach(f => (0 until Valentines).foreach(v => table.setColorAt(Color.PINK, f, v)))
  
  f.createFrame("Shipments of Flowers").add(table)
  f.pack()
  
  Thread.sleep(2000) // Delay to allow the GUI pre-solve to be visible.
  

  // The decision vars are binary: does a shop send a bouquet to a valentine or not?
  val x = Array.tabulate(Florists, Valentines)((f, v) => LPFloatVar(s"x($f,$v)", 0, 1))

  minimize(sum(0 until Florists, 0 until Valentines)((f, v) => x(f)(v) * cost(f)(v)))
  (0 until Florists).foreach(f => add(sum(0 until Valentines)(v => x(f)(v)) == stock(f)))
  (0 until Valentines).foreach(v => add(sum(0 until Florists)(f => x(f)(v)) == requests(v)))
  start()

  // Double check we actually found the optimal solution!
  assert(objectiveValue.get == 38)
  assert(status == LPStatus.OPTIMAL)
  
  release() // Frees up the memory held by the external solver.
  
  // Update the GUI to show the (romantic) solution. 
  (0 until Florists).foreach(f => table.setValueAt(sum(0 until Valentines)(v => x(f)(v)).value.get.toString, f, Valentines))
  (0 until Valentines).foreach(v => table.setValueAt(sum(0 until Florists)(f => x(f)(v)).value.get.toString, Florists, v))
  (0 until Florists).foreach(f => (0 until Valentines).filter(x(f)(_).value.get.ceil < 1).foreach(v => table.setColorAt(Color.WHITE, f, v)))
  table.setValueAt(objectiveValue.get.toString, Florists + 2, Valentines)
}

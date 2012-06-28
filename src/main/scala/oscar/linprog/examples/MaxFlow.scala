/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package oscar.linprog.examples


import oscar.linprog.modeling._

/**
 *  Note: example taken from glpk
 *  The Maximum Flow Problem in a network G = (V, E), where V is a set
 *  of nodes, E within V x V is a set of arcs, is to maximize the flow
 *  from one given node s (source) to another given node t (sink) subject
 *  to conservation of flow constraints at each node and flow capacities
 *  on each arc.
 */
object MaxFlow extends LPModel {

	
  def main(args: Array[String]) = {  
	  

	 val lp = LPSolver(LPSolverLib.glpk)
	 
	 val Lines = 0 to 7
     val Columns = 0 to 8
     val nbcol = Columns.size
     val nbline = Lines.size
	 val capa = Array(Array(0,   12,   0, 23,   0,  0,  0,  0,  0),
				      Array( 0,   0,  10,  9,   0,  0,  0,  0,  0),
					  Array( 0,   0,   0,  0,  12,  0,  0, 18,  0),
					  Array( 0,   0,   0,  0,  26,  0,  0,  0,  0),
					  Array( 0,  11,   0,  0,  0,  25,  4,  0,  0),
					  Array( 0,   0,   0,  0,  0,   0,  7,  8,  0),
					  Array( 0,   0,   0,  0,  0,   0,  0,  0, 15),
					  Array( 0,   0,   0,  0,  0,  63,  0,  0, 20))

	val x = Array.tabulate(nbline,nbcol) ((l,c) => LPVar(lp,"x"+(l,c), 0,capa(l)(c) ))					 
						 
	lp.maximize(sum(Lines)(l => x(l)(nbcol-1))) subjectTo {
		 for(l <- 1 to nbline-1)
		     lp.add(sum(Columns)(c => x(l)(c)) - sum(Lines)(c => x(c)(l) ) == 0)
	}
  
	println("objective: "+lp.getObjectiveValue())
	for(l <- Lines){
	  for(c <- Columns)
		print(x(l)(c).getValue+"  ")
	  println()  
	}
	lp.release()

  }
  
}

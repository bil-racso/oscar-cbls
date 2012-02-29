/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples


import scampi.linprog.modeling._
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
object Steel extends LPModel with MIPModel{

  class Column (val x : LPVar,val capa : Int, val contains : Array[Int]) {
	  override def toString() : String = {
	 	  contains.mkString("\t")
	  }   
	  def number() : Int = Math.ceil(x.getValue).toInt
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
        case w :: c :: v => vals_ = vals_.drop(2)
        weight = weight :+ w
        col = col :+ c - 1 //color starts at 1 in input file
        case Nil => Unit
      }
    }
    (capa toArray, weight, col)
  }


  def main(args: Array[String]): Unit = { 

	  val (capa,weight,col) = readData()
	  val (nbCapa,nbSlab,nbCol) = (capa.length, weight.length, col.max+1)
	  val Slabs = 0 until nbSlab
	  val Cols = 0 until nbCol
	  val loss = (0 to capa.max).map(c => capa.filter(_ >= c).min - c)
	  val colorOrders = Cols.map(c => (Slabs).filter (s => col(s) == c))
		

	  val lp = LPSolver(LPSolverLib.lp_solve)
				
	  var C : Array[Column] = Array()
		
	  val Capa = Array.tabulate(capa(capa.size-1))(_ => 0)


	  for (s <- 0 until nbSlab) {
		  val cont = Array.tabulate(nbSlab)(_ => 0)
		  cont(s) = 1
		  C = C :+ new Column(LPVar(lp,"x",0,10000),loss(weight(s)), cont)
	  }
		
	  var meet : Array[LPConstraint] = Array()
		
	  // Master Problem 
	  lp.minimize(sum(C)(c => c.x*c.capa)) subjectTo {
	 	  for (s <- 0 until nbSlab) {
	 		  meet = meet :+ lp.add(sum(C)(c => c.x*c.contains(s)) == 1)
	 	  }
	 }

		
	  // Pricing Problem
	  var added=false	
	  for (CAPA <- capa) {		  
	      do{
	    	  added = false
	    	  val mip = MIPSolver(LPSolverLib.lp_solve)

	    	  val use = Array.tabulate(nbSlab)(_ => MIPVar(mip,"use",0 to 1))
	    	  val v = Array.tabulate(nbCol)(_ => MIPVar(mip,"v",0 to 1))
	    	  val cost = Array.tabulate(nbSlab)(meet(_).getDual)

	    	  mip.maximize(sum(Slabs)(s => ((cost(s)+weight(s)) * use(s)))) subjectTo {

	    		  mip.add(sum(Slabs)(s => use(s)*weight(s)) <= CAPA)	
	    		  for (s <- Slabs)
	    			  mip.add(use(s)<=v(col(s)))
	    		  mip.add(sum(Cols)(c => v(c)) <= 2)  		
	    	  }

	    	  if (mip.getObjectiveValue() > CAPA + 0.01) {	 	  		 	  		  
	    		  var a = new Array[scala.Double](nbSlab)
	    		  var tmp=0
	    		  for( i <- 0 until nbSlab){
	    			  a(i) = use(i).getValue.toInt*loss(weight(i))
	    			  tmp += (use(i).getValue.toInt*weight(i))
	    		  }

	    		  val x = lp.addColumn(loss(tmp),meet,use.map(_.getValue)) //create a new variable by introducing a new column
	    		  C = C :+ new Column(x,loss(tmp), use.map(_.getValue.toInt))
	    		  added = true
	    	  }
	    	  
	    	  println("==> master obj:"+lp.getObjectiveValue())
	 	 
	      } while(added)
	  }	  
	  println("objective: " + lp.getObjectiveValue())
	  println("nbColumns: " + C.size)
	} // end of main
}

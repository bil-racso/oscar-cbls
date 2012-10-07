package oscar.cp.mem

import scala.collection.mutable.Set
import scala.collection.mutable.Queue

/** Some ways to calculate the hypervolume of a Pareto set.
 * 
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
object Hypervolume {
		
	type ParetoPoint = Array[Double]
	type ParetoSet   = Array[ParetoPoint]
	
	def simple2DHypervolume(ps : ParetoSet, ref : ParetoPoint) : Double = {
		
		if(ps.isEmpty)
			0.0
			
		else if(ps(0).size > 2 || ref.size > 2)
			throw new IllegalArgumentException("This algorithm only works with two dimensional points.")
		
		else simple2DHypervolume0(ps.sortWith(nonDecreasing(0)), ref, 0, 0.0)
	}
	
	private def simple2DHypervolume0(ps : ParetoSet, ref : ParetoPoint, i : Int, vol : Double) : Double = {
			
		if (i == ps.size)
			vol
		else {
			val x = ps(i)(0) - (if(i == 0) ref(0) else ps(i-1)(0))
			val y = ps(i)(1) - ref(1)
			simple2DHypervolume0(ps, ref, i+1, vol + x*y)
		}
	}
	
	private def nonDecreasing(obj : Int) = (p1 : ParetoPoint, p2 : ParetoPoint) => p1(obj) < p2(obj)
}
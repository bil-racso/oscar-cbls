package oscar.cp.mem

import scala.collection.mutable.Set
import scala.collection.mutable.Queue

/** Implementation of the Hypervolume by Slicing Objectives (HSO) algorithm. 
 * 
 *  @author Renaud Hartert ren.hartert@gmail.com
 */
object HyperVolume {
	
	
	type ParetoSet   = Array[Array[Double]]
	type ParetoPoint = Array[Double]
	
	val refPoint : ParetoPoint = null
	
	def abs(d : Double) = if (d < 0) -d else d
	
	def worsening(o : Int) = (i : ParetoPoint, j : ParetoPoint) => i(o) < j(o)
	
	def hypervolumeSO(ps : ParetoSet) {
		
		val p0 = ps.sortWith(worsening(0))
		val s : Set[(Int, ParetoSet)] = Set((0, p0))
		
		for (k <- 0 until ps(0).size) {
			
			val sPrim : Set[(Int, ParetoSet)] = Set()
			
			for((x, q0) <- s) {
				
				val sl = slice(q0, k)
			}
		}
		
	}
	
	def slice(p0 : ParetoSet, k : Int) = {
		
		var p  = p0.head
		var p1 = p0.tail
		
		var q1 : Queue[(ParetoPoint, Int)] = Queue()
		
		val s : Set[(Double, Queue[(ParetoPoint, Int)])] = Set()
		
		while(!p1.isEmpty) {
			
			q1 enqueue ((p, k+1))
			val pBis = p1.head
			s add ((abs(p(k) - pBis(k)), q1))
			
			p = pBis
			
			p1 = p1.tail
		}
		
		q1 enqueue ((p, k+1))
		s add ((abs(p(k) - refPoint(k)), q1))
		
		s
	}

	def insert(p : ParetoPoint, k : Int, p0 : ParetoSet) {
		
		var q1 : Queue[ParetoPoint] = Queue()
		var p1 = p0
		
		while (!p1.isEmpty && p1.head(k) > p(k)) {
			
			q1 enqueue p1.head
			p1 = p1.tail
		}
		
		q1 enqueue p
		
		while(!p1.isEmpty) {
			
			if (!(p(k) > p1.head(k))) {
				
				q1 enqueue p1.head
			}
		}
		
		
	}
}
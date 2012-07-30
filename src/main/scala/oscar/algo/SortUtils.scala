package oscar.algo

import scala.reflect.ClassManifest
import scala.math.Ordering

object SortUtils {
  
	def stableSort[K: ClassManifest](a: Array[K], l : Int, r : Int, f: (K, K) => Boolean) = {

		stableSort1(a, l, r-1, new Array[K](a.length), f)
	}

	private def stableSort1[K : ClassManifest](a: Array[K], lo: Int, hi: Int, scratch: Array[K], f: (K,K) => Boolean) {
	    if (lo < hi) {
			val mid = (lo+hi) / 2
			stableSort1(a, lo, mid, scratch, f)
			stableSort1(a, mid+1, hi, scratch, f)
			var k, t_lo = lo
			var t_hi = mid + 1
			while (k <= hi) {
			    if ((t_lo <= mid) && ((t_hi > hi) || (!f(a(t_hi), a(t_lo))))) {
			    	scratch(k) = a(t_lo)
			    	t_lo += 1
			    } else {
			    	scratch(k) = a(t_hi)
			    	t_hi += 1
			    }
			    k += 1
			}
			k = lo
			while (k <= hi) {
			    a(k) = scratch(k)
			    k += 1
			}
	    }
	}
	
	def main(args: Array[String]) {
		
		val a = Array(5,6,2,9,6,8,3)
		stableSort(a, 0, 4, (a : Int,b:Int) => a < b)
		println(a.mkString(","))
	}
}
package oscar.algo

package object reversible {
  
  implicit def revBool2Bool(rb: ReversibleBool): Boolean = rb.getValue
  implicit def revInt2Int(ri: ReversibleInt): Int = ri.getValue
  
}

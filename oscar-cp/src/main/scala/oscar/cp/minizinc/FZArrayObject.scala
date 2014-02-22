package oscar.cp.minizinc

import oscar.cp.core.CPVar

abstract class FZArrayObject ( 
    val value: Set[Int],
    val annotations: List[Annotation],
    val variables: Array[Variable],
    override val name : String)  extends FZObject(name) {
  


}

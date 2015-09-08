package oscar.cp.minizinc

import oscar.cp.core.variables.CPVar

class FZVarObject ( 
    val annotations: List[Annotation],
    val cpvar: CPVar,
    override val name : String)  extends FZObject(name) {

}

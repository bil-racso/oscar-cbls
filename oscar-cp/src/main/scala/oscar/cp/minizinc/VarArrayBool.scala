package oscar.cp.minizinc

import oscar.cp.core.variables.CPBoolVar

class VarArrayBool (
    val annotations: List[Annotation],
    val cpvar: Array[CPBoolVar],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar.mkString(",")
	
}

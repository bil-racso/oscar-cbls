package oscar.cp.minizinc

import oscar.cp.core.CPVarBool

class VarArrayBool (
    val annotations: List[Annotation],
    val cpvar: Array[CPVarBool],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar.mkString(",")
	
}
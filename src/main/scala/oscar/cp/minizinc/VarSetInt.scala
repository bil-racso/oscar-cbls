package oscar.cp.minizinc

import oscar.cp.core.CPVarSet

class VarSetInt (
    //val value: Set[Int],
    val annotations: List[Annotation],
    val cpvar: CPVarSet,
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar
	
}
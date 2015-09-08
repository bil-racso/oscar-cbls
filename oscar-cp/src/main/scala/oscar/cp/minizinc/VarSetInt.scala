package oscar.cp.minizinc

import oscar.cp.core.variables.CPSetVar

class VarSetInt (
    //val value: Set[Int],
    val annotations: List[Annotation],
    val cpvar: CPSetVar,
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar
	
}

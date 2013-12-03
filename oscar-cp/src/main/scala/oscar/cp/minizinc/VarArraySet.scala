package oscar.cp.minizinc

import oscar.cp.core.CPVarSet

class VarArraySet (
    val value: Set[Int],
    val annotations: List[Annotation],
    val cpvar: Array[CPVarSet],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar.mkString(",")
	
}

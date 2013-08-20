package oscar.cp.minizinc

import oscar.cp.core.CPVarInt

class VarArrayIntRange (
    val value: Set[Int],
    val range: Boolean,
    val annotations: List[Annotation],
    val cpvar: Array[CPVarInt],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + range + " " + cpvar.mkString(",")
	
}
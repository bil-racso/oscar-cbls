package oscar.cp.minizinc

import oscar.cp.core.CPVarInt

class VarIntRange (
    val value: Set[Int],
    val annotations: List[Annotation],
    val cpvar: CPVarInt,
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations
}
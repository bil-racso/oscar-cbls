package oscar.cp.minizinc

import oscar.cp.core.CPVarInt

class VarIntRange (
    val range: Range,
    val annotations: List[Annotation],
    val cpvar: CPVarInt,
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + range
}
package oscar.cp.minizinc

import oscar.cp.core.CPVarInt

class VarInt (
    //var value: Int,
    val annotations: List[Annotation],
    val cpvar: CPVarInt,
    override val name: String
    ) extends FZObject (name){
	
    override def toString() = name + " " + annotations + " " + cpvar
}
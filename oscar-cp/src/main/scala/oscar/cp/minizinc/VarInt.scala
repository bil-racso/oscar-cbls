package oscar.cp.minizinc

import oscar.cp.core.variables.CPIntVar

class VarInt (
    //var value: Int,
    val annotations: List[Annotation],
    val cpvar: CPIntVar,
    override val name: String
    ) extends FZObject (name){
	
    override def toString() = name + " " + annotations + " " + cpvar
}

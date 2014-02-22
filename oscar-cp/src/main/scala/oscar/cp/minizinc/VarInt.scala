package oscar.cp.minizinc

import oscar.cp.core.CPIntVar

class VarInt (
    val annotations: List[Annotation],
    val variable: Variable,
    override val name: String
    ) extends FZObject (name){
	
    override def toString() = name + " " + annotations + " " + variable
}

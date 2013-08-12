package oscar.cp.minizinc

import oscar.cp.core.CPVarBool

class VarBool (
    //var value: Boolean, 
    val annotations: List[Annotation],
    val cpvar: CPVarBool,
    override val name: String
    ) extends FZObject (name){

	override def toString() = name + " " + annotations + " " + cpvar
}
package oscar.cp.minizinc

import oscar.cp.core.CPVarBool

class VarBool (
    //var value: Boolean, 
    override val annotations: List[Annotation],
    override val cpvar: CPVarBool,
    override val name: String
    ) extends FZVarObject (annotations, cpvar, name){

	override def toString() = name + " " + annotations + " " + cpvar
}
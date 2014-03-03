package oscar.cp.minizinc

import oscar.cp.core.CPBoolVar

class VarBool (
    //var value: Boolean, 
    override val annotations: List[Annotation],
    override val cpvar: CPBoolVar,
    override val name: String
    ) extends FZVarObject (annotations, cpvar, name){

	override def toString() = name + " " + annotations + " " + cpvar
}

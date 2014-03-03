package oscar.cp.minizinc

import oscar.cp.core.CPIntVar

class VarArrayInt (
    override val value: Set[Int],
    override val annotations: List[Annotation],
    override val cpvar: Array[CPIntVar],
    override val name: String
    ) extends FZArrayObject (value, annotations, cpvar, name) {

	override def toString() = name + " " + annotations + " " + cpvar.mkString(",")
	
}

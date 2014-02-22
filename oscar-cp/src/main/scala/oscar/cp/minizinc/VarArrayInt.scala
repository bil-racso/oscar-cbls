package oscar.cp.minizinc

import oscar.cp.core.CPIntVar

class VarArrayInt (
    override val value: Set[Int],
    override val annotations: List[Annotation],
    override val variables: Array[Variable],
    override val name: String
    ) extends FZArrayObject (value, annotations, variables, name) {

	override def toString() = name + " " + annotations + " " + variables.mkString(",")
	
}

package oscar.cp.minizinc

import oscar.cp.core.CPSetVar

class VarArraySet (
    val value: Set[Int],
    val annotations: List[Annotation],
    val cpvar: Array[CPSetVar],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + cpvar.mkString(",")
	
}

package oscar.cp.minizinc

class VarIntRange (
    val range: Range,
    val annotations: List[Annotation],
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotations + " " + range
}
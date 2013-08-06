package oscar.cp.minizinc

class VarIntRange (
    val range: Range,
    val annotation: String,
    override val name: String
    ) extends FZObject (name) {

	override def toString() = name + " " + annotation + " " + range
}
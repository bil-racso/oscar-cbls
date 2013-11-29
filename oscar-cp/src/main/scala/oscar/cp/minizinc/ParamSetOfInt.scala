package oscar.cp.minizinc

class ParamSetOfInt (
    val value: Set[Int],
    val range: Boolean,
    override val name: String
    ) extends FZObject (name) {

  	override def toString() = name + " " + value + " " + range
}

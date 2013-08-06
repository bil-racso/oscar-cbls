package oscar.cp.minizinc

class VarInt (
    var value: Int,
    val annotations: List[Annotation],
    override val name: String
    ) extends FZObject (name){
	
    override def toString() = name + " " + annotations + " " + value
}
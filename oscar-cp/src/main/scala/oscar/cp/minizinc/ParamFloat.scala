package oscar.cp.minizinc

class ParamFloat (
    val value: Float, 
    override val name: String
    ) extends FZObject (name){
	
	override def toString() = name + " " + value
}

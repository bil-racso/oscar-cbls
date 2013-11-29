package oscar.cp.minizinc

class ParamBool (
    val value: Boolean, 
    override val name: String
    ) extends FZObject (name){
	
	override def toString() = name + " " + value
}

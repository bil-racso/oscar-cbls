package oscar.cp.minizinc

class ParamInt (
    val value: Int, 
    override val name: String
    ) extends FZObject (name){
	
	override def toString() = name + " " + value
}
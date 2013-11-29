package oscar.cp.minizinc

class ParamArrayBool (
    val value: Any, 
    val range: Any,
    override val name: String
    ) extends FZObject (name){
	
  
    override def toString() = name + " " + value + " " + range
}

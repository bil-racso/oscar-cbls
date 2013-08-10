package oscar.cp.minizinc

class ParamArrayFloat (
    //is that useful to create several data structures ?
    val value: Any, 
    val range: Any,
    override val name: String
    ) extends FZObject (name){
	
  
    override def toString() = name + " " + value + " " + range
}
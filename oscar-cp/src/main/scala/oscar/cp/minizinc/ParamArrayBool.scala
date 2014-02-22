package oscar.cp.minizinc

class ParamArrayBool (
    val values: Array[Boolean],
    override val name: String
    ) extends FZObject (name){
	
  
    override def toString() = name + " " + values.mkString(",")
}

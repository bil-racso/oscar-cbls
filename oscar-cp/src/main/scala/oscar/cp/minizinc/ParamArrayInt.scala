package oscar.cp.minizinc

class ParamArrayInt (
    val values: Array[Int],
    override val name: String
    ) extends FZObject (name){
	
  
    override def toString() = name + " " + values.mkString(",")
}

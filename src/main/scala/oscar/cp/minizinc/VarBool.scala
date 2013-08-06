package oscar.cp.minizinc

class VarBool (
    var value: Boolean, 
    val annotation: String,
    override val name: String
    ) extends FZObject (name){
  
	def setValue (v: Boolean) {
      value = v
    }
}
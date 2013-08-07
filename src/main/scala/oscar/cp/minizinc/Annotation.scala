package oscar.cp.minizinc

class Annotation (
    val name: String,
    val args: List[Any]
	) {
	
	override def toString() = name + " " + args
}
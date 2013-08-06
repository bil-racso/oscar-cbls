package oscar.cp.minizinc

class Annotation (
    name: String,
    args: List[Any]
	) {
	
	override def toString() = name + " " + args
}
package oscar.flatzinc.model

class Annotation (
    val name: String,
    var args: List[Any]
	){
    def this(name: String) = this(name,List.empty[Any]);
    def add(e: Any){
      args = e :: args;
    }
	override def toString() = name + " " + args
}

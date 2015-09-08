package oscar.cbls.invariants.core.propagation

trait SymmetricType {
  val selfAsPropagationElement : PropagationElement
  val definesSymmetries : Boolean
  def localSymmetries : SymmetryClasses
}

trait Asymmetric extends SymmetricType{
  override val definesSymmetries = false
  override def localSymmetries = {
    val syms = new SymmetryClasses()
    for(s <- selfAsPropagationElement.getStaticallyListenedElements){
      syms.addPropagationElement(s, syms.nextClassId)
    }
    println("Element foireux asymétrique : " + this)
    syms.println
    println("\n\n")
    syms
  }
}

trait Symmetric extends SymmetricType{
  override val definesSymmetries = true

  override def localSymmetries = {
    val syms = new SymmetryClasses()
    for(s <- selfAsPropagationElement.getStaticallyListenedElements) {
      syms.addPropagationElement(s, 1)
    }
    syms
  }
}

trait PartiallySymmetric extends SymmetricType{
  override val definesSymmetries = true
}

trait NotSubjectToSymmetries extends SymmetricType{
  override val definesSymmetries = false
  override val localSymmetries = null
}
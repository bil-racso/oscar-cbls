package oscar.cp.lcg.core

abstract sealed class LiftedBoolean { 
  def opposite: LiftedBoolean 
}

object False extends LiftedBoolean { 
  @inline final override val opposite: LiftedBoolean = True 
  final override val toString: String = "False"
}

object True extends LiftedBoolean { 
  @inline final override val opposite: LiftedBoolean = False 
  final override val toString: String = "True"
}

object Unassigned extends LiftedBoolean { 
  @inline final override val opposite: LiftedBoolean = this
  final override val toString: String = "Unassigned"
}
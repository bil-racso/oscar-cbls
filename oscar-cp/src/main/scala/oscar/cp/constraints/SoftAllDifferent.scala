package oscar.cp.constraints

import oscar.cp.core.Constraint
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPOutcome
import oscar.cp.core.variables.CPIntVarViewMinus
import oscar.cp.core.variables.CPIntVarViewOffset
import oscar.cp.core.variables.CPIntVar

/** author Renaud Hartert ren.hartert@gmail.com */
final class SoftAllDifferent(variables: Array[CPIntVar], violations: CPIntVar) extends Constraint(violations.store, "SoftAllDifferent") {

  final override def setup(l: CPPropagStrength): CPOutcome = {
    val nVariables = variables.length
    val nValues = new CPIntVarViewOffset(new CPIntVarViewMinus(violations), nVariables)
    if (l == CPPropagStrength.Weak) s.post(new AtLeastNValueFWC(variables, nValues))
    else if (l == CPPropagStrength.Medium) s.post(new AtLeastNValueFWC(variables, nValues))
    else if (l == CPPropagStrength.Medium) s.post(new AtLeastNValueAC(variables, nValues))
    else sys.error("Unknown propagation level.")
  } 
}
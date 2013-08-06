package oscar.cp.minizinc

object FZType extends Enumeration {
    type FZType = Value
	val P_BOOL, 
		P_FLOAT, 
		P_INT,
		P_SET_INT,
		P_ARRAY_BOOL,
		V_BOOL,
		V_FLOAT,
		V_INT,
		V_INT_RANGE = Value
}
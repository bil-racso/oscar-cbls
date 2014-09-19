package oscar.flatzinc

class NoSuchConstraintException(val cstr: String, val where: String) extends Exception("Constraint "+cstr+" is not implemented in "+where)

class UnsatException(val txt: String) extends Exception("The problem is unsatisfiable: "+txt)

class ParsingException(val txt: String) extends RuntimeException("Parsing Exception: "+txt)
package oscar.cp.test.minizinc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.minizinc.Parser

class TestVarDecl extends FunSuite with ShouldMatchers {
  
	var input: String = " "
	    
	test ("Test var bool") {
		val p = new Parser()
    	input = "bool : b;"
		p.parseVar(input)
		//println(p.model.dict.toString)
	}
	
	test ("Test var int_const..int_const") {
		val p = new Parser()
		input = "array [1..4] of var 1..8: i;"
		p.parseVar(input)
		//println(p.model.dict.toString)
	}

}
package oscar.cp.test.minizinc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.minizinc.Parser


class TestParamDecl extends FunSuite with ShouldMatchers {
  
	var input: String = " "
	  
	  
	test ("Test param bool") {
		val p = new Parser()
    	input = "bool : b = false;"
		p.parseParam(input)
		p.model.dict.toString should be("Map(b -> (P_BOOL,b false))")
	}
	
	
	test ("Test param Float1") {
		val p = new Parser()
		input = "float: f1 = 5.2;"
		p.parseParam(input)
		p.model.dict.toString should be("Map(f1 -> (P_FLOAT,f1 5.2))")
	}
	
	test ("Test param Float2") {
		val p = new Parser()
		input = "float: f2 = 5.2e3;"
		p.parseParam(input)
		p.model.dict.toString should be("Map(f2 -> (P_FLOAT,f2 5200.0))")
	}
	
	test ("Test param Float3") {
		val p = new Parser()
		input = "float: f3 = 5E3;"
		p.parseParam(input)
		p.model.dict.toString should be("Map(f3 -> (P_FLOAT,f3 5000.0))")
	}
	
	
	test ("Test param Int") {
		val p = new Parser()
		input = "int: i = 42;"
		p.parseParam(input) 
		p.model.dict.toString should be("Map(i -> (P_INT,i 42))")
	}
	
	
	test ("Test param Set of Int1") {
		val p = new Parser()
		input = "set of int: sI1 = {4,6,9};"
		p.parseParam(input) 
		p.model.dict.toString should be("Map(sI1 -> (P_SET_INT,sI1 List(4, 6, 9) false))")
	}
	
	test ("Test param Set of Int2") {
		val p = new Parser()
		input = "set of int: sI2 = 4..7;"
		p.parseParam(input) 
		p.model.dict.toString should be("Map(sI2 -> (P_SET_INT,sI2 Range(4, 5, 6, 7) true))")
	}
	
}
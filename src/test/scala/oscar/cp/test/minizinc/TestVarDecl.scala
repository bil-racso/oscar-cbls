package oscar.cp.test.minizinc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.minizinc.Parser

class TestVarDecl extends FunSuite with ShouldMatchers {
  
	var input: String = " "
	  
	// VarBool
	test ("Test var bool") {
		val p = new Parser()
    	input = "var bool: b;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(b -> (V_BOOL,b List() false,true))")
	}
	
	//VarInt
	test ("Test var int") {
		val p = new Parser()
		input = "var int: i;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(i -> (V_INT,i List() -10000..10000))")
	}
	
	test ("Test var int_const..int_const1") {
		val p = new Parser()
		input = "var 2..3: ii1;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(ii1 -> (V_INT,ii1 List() 2..3))")
	}
	
	test ("Test var int_const..int_const2") {
		val p = new Parser()
		input = "var 2..3: ii2 :: output_var;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(ii2 -> (V_INT,ii2 List(output_var null) 2..3))")
	}

	test ("Test var {int_const, ...}") {
		val p = new Parser()
		input = "var {2, 5, 6}: is;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(is -> (V_INT,is List() ReversibleSparseSet(2, 6, 5)))")
	}
	
	//VarSet
	test("Test var set of int_const..int_const") {
		val p = new Parser()
		input = "var set of 2..4: s1;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(s1 -> (V_SET_INT,s1 List() Set() Set(2, 3, 4)))")
	}
	
	test("Test var set of {int_const, ...}") {
		val p = new Parser()
		input = "var set of {0, 3, 9, 5}: s2;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(s2 -> (V_SET_INT,s2 List() Set() Set(0, 9, 5, 3)))")
	}
	
	//arrays
	
	//array of VarBool
	test ("Test array of var bool") {
		val p = new Parser()
		input = "array [1..2] of var bool: aB;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aB -> (V_ARRAY_BOOL,aB List() false,true,false,true))")
	}
	
	//arrays of VarInt
	test ("Test array of var int") {
		val p = new Parser()
		input = "array [1..2] of var int: aI;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aI -> (V_ARRAY_INT,aI List() -10000..10000,-10000..10000))")
	}
	
	test ("Test array of var int output") {
		val p = new Parser()
		input = "array [1..2] of var int: aI :: output_array([1..2]);"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aI -> " +
				"(V_ARRAY_INT,aI List(output_array List(List(Range(1, 2)))) -10000..10000,-10000..10000))")
	}
	
	test ("Test array of var int_const..int_const") {
		val p = new Parser()
		input = "array [1..4] of var 1..8: aI1;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aI1 -> (V_ARRAY_INT,aI1 List() 1..8,1..8,1..8,1..8))")
	}
	
	test ("Test array of var {int_const, ...}") {
		val p = new Parser()
		input = "array [1..4] of var {5, 6, 2}: aI2;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aI2 -> (V_ARRAY_INT,aI2 List() ReversibleSparseSet(2, 6, 5)," +
				"ReversibleSparseSet(2, 6, 5),ReversibleSparseSet(2, 6, 5),ReversibleSparseSet(2, 6, 5)))")
	}
	
	test ("Test array of set of int_const..int_const") {
		val p = new Parser()
		input = "array [1..2] of var set of 2..4: aS1;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aS1 -> " +
				"(V_ARRAY_SET,aS1 List() Set() Set(2, 3, 4),Set() Set(2, 3, 4)))")
	}
	
	test ("Test array of var set of {int_const, ...}") {
		val p = new Parser()
		input = "array [1..2] of var set of {5, 3}: aS2;"
		p.parseVar(input)
		p.model.dict.toString should be("Map(aS2 -> " +
				"(V_ARRAY_SET,aS2 List() Set() Set(3, 5),Set() Set(3, 5)))")
	}
}
package oscar.flatzinc.test

import org.scalatest.FunSuite
import org.scalatest.Matchers
import oscar.flatzinc.parser.FZParser
import oscar.flatzinc.cbls.Log
import oscar.flatzinc.parser.Options
import oscar.flatzinc.ParsingException
import oscar.flatzinc.model.FZProblem
import oscar.flatzinc.NoSuchConstraintException

class TestParser extends FunSuite with Matchers{

  def getSATModel(modelStr: String): FZProblem = {
    val opts = new Options(false,Array(""))
    val log = new Log(opts)
    val model = FZParser.readFlatZincModelFromString(modelStr+" solve satisfy;", log)
    model.problem
  }
  test("test no solve"){
    val opts = new Options(false,Array(""))
    val log = new Log(opts)
    
    a [ParsingException] should be thrownBy {
      FZParser.readFlatZincModelFromString("var int: x;", log)      
    }
  }
  test("test empty model"){
    val problem = getSATModel("")
    problem.variables.length should be(0)
    problem.constraints.size should be(0)
  }
  
  test("test simple model no constraint"){
    val problem = getSATModel("var int: x; ")
    problem.variables.length should be(1)
    problem.constraints.size should be(0)
  }
  
  test("test alias"){
    val problem = getSATModel("var int: x; var int: y = x;")
    problem.variables.length should be(1)
    problem.constraints.size should be(0)
  }
  
  test("test simple model"){
    val problem = getSATModel("var int: x; var int: y;\n"+ 
        "constraint int_eq(x,y);");
    problem.variables.length should be(2)
    problem.constraints.size should be(1)
  }
  test("test no constraint"){
    a [NoSuchConstraintException] should be thrownBy {
      getSATModel("var int: x; var int: y;\n"+ 
        "constraint my_constraint_does_not_exist(x,y);");
    }
  }
  
  test("test 2.0"){
    
/*var 1..8: X_INTRODUCED_0;
var 1..8: X_INTRODUCED_1;
var 1..8: X_INTRODUCED_2;
array[1..3] of var 1..8: q:: output_array([1..8]) = [X_INTRODUCED_0,X_INTRODUCED_1,X_INTRODUCED_2];*/
    val problem = getSATModel("""
        array[1..3] of var 1..8: q :: output_array([1..8]);
        array[1..1] of int: x = [1,-1];
""")
  }
}
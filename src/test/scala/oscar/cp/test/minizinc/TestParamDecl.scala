package oscar.cp.test.minizinc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.minizinc.Parser


// /!\ check if myParseAll in oscar.cp.minizinc.Parser tries to parse a flat_zinc model
class TestParamDecl extends FunSuite with ShouldMatchers {
	 test ("mytest"){
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	val input = "bool : name = false; solve satisfy;"
		println(new Parser().myParseAll(input))
		//println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}
}
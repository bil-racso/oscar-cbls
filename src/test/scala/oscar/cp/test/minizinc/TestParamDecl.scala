package oscar.cp.test.minizinc

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.minizinc.Parser

class TestParamDecl extends FunSuite with ShouldMatchers {
	def test {
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	val input = "bool : name = false;"
		//println(parseAll(param_decl, input))
		//println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}
}
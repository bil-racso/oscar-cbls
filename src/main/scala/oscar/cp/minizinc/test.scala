package oscar.cp.minizinc

import java.io.FileReader

object test extends Parser with App{
    test
   
	def test {
		//val input = "bool: name = true;"
    	val input = new FileReader(args(0))
    	//val input = "array [1..2] of var 1..2: queens :: output_array([1..2]);" +
    			"constraint int_lin_ne([1, 1], [queens[1], queens[2]], 2);" +
    			"constraint int_lin_ne([1, 1], [queens[1], queens[2]], 3);" +
    			"solve satisfy;"
		//println(myParseAll(input))
		myParseAll(input)
		//println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}	
}
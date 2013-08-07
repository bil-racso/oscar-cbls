package oscar.cp.minizinc

object test extends Parser with App{
    test
   
	def test {
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	val input = "var 2..3: x :: output_var;"+
    			"var 1..4: y :: output_var;" +
    			"constraint int_le(x, y);" +
    			"constraint set_ne(x, y);" +
    			"solve satisfy;"
		println(myParseAll(input))
		//myParseAll(input)
		println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}	
}
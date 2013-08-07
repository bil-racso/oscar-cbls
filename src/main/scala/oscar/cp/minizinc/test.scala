package oscar.cp.minizinc

object test extends Parser with App{
    test
   
	def test {
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	val input = "bool: b = true;" +
    			"int: i = 42;" +
    			"float: f1 = 2.3;" +
    			"float: f2 = 2.3e2;" +
    			"float: f3 = 2e3;" +
    			"solve satisfy;"
		//println(myParseAll(input))
		myParseAll(input)
		println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}	
}
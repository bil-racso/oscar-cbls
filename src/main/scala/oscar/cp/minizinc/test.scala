package oscar.cp.minizinc

object test extends Parser with App{
    test
   
	def test {
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	val input = "array [1..4] of var 1..8: queens :: output_array([1..4]); solve satisfy;"
		println(myParseAll(input))
		//myParseAll(input)
		println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}	
}
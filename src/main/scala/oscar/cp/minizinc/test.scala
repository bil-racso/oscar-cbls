package oscar.cp.minizinc

object test extends Parser with App{
    test
   
	def test {
		//val input = "bool: name = true;"
    	//val input = new FileReader(args(0))
    	val input = args(0)
		println(parseAll(var_decl, input))
		//println(model.dict.toString)
		//println(model.dict.get("name"))
//		model.dict.get("name") match {
//		  case Some(p) => println(p.toString)
//		}
	}	
}
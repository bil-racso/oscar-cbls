package oscar.examples.cp.BinPacking.DataStructures

class Results {
	var profileKeys = List[String]()
	var valuesKeys = List[String]()
	var results : List[(List[Double],List[Double])] = Nil
	
	def readFile(path : String)
	{
	  val xml = scala.xml.XML.loadFile(path)
	  
	}
	
	def writeInFile(path : String)
	{
		val xml =
			<results>
				<profile>{for (key <- profileKeys) yield <key>{key}</key>}</profile>
				<values>{for (key <- valuesKeys) yield <key>{key}</key>}</values>
				{for (result <- results) 
					yield <result>
						<profile>{result._1.mkString("\t")}</profile>
		  				<values>{result._2.mkString("\t")}</values>
					</result>
				}
				</results>
		scala.xml.XML.save(path,xml)
	}
}

object Results {
  def main(args : Array[String]) {
    println("writing test Results file in test_result.xml")
    val r = new Results
    r.profileKeys = List("profile key1","profile key2","profile key3","profile key4")
    r.valuesKeys = List("value key1","value key2","value key3")
    val values = Stream.iterate(0.0)(_+0.1)
    r.results = (for (i <- 1 to 20) yield (values.take(r.profileKeys.length).toList, values.take(r.valuesKeys.length).toList)).toList
    r.writeInFile("test_result.xml")
    
  }
  
}

package oscar.cp.multiobjective

object VRPTWParser {

	def parse(filepath : String) : InstanceVRPTW = {
		
		if (true) {
			throw new Exception()
		}
		
		null
		
	}
}

case class InstanceVRPTW(n : Int,
						 k : Int,
						 c : Int,
						 demand : Array[Int],
						 twStart : Array[Int],
						 twEnd : Array[Int],
						 servDur : Array[Int],
						 dist   : Array[Array[Int]])
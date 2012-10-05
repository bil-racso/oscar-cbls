package oscar.cp.scheduling

class FixedActivity(idx : Int, private var s : Int, 
							   private var e : Int, 
							   private var i : Int, 
							   private var m : Int) {

	def start = s
	def start_= (x : Int) {s = x}
	
	def end = e
	def end_= (x : Int) {e = x}
	
	def inc     = i
	def inc_= (x : Int) {i = x}
	
	def machine = m
	def machine_= (x : Int) {m = x}
	
	def id      = idx
	
	override def toString() = "<id: " + idx + ", start: " + s + ", end: " + e + ", machine: " + m + ", inc: " + i + ">"
}

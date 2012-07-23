package oscar.cp.scheduling

class FixedActivity(idx : Int, s : Int, e : Int, i : Int, m : Int) {

	def start   = s
	def end     = e
	def inc     = i
	def id      = idx
	def machine = m
}
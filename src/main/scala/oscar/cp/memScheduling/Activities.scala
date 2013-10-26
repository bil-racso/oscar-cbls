package oscar.cp.memScheduling

object Activities {
	
  def apply(durations : Array[ImplicitVarInt])(implicit scheduler: CPScheduler): Array[Activity] = {
	  Array.tabulate(durations.length)(i => Activity(durations(i))(scheduler))
	}
}
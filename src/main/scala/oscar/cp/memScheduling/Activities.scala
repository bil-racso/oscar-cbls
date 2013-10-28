package oscar.cp.memScheduling

object Activities {
	
  def apply(durations : Array[ImplicitVarInt])(implicit scheduler: CPScheduler): Array[Activity] = {
	  Array.tabulate(durations.length)(i => Activity(durations(i))(scheduler))
	}
  
  def apply(durations : Array[ImplicitVarInt], earlypen: Array[Int], tardipen: Array[Int])(implicit scheduler: CPScheduler): Array[ActivityET] = {
	  Array.tabulate(durations.length)(i => ActivityET(durations(i), earlypen(i), tardipen(i))(scheduler))
	}
}
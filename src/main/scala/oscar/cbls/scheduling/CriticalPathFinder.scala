package oscar.cbls.scheduling

import oscar.cbls.search.SearchEngine

/**
 * finds a critical path for the given planning
 *
 */
object CriticalPathFinder extends SearchEngine{

  /**returns a critical path in the planning
    *
    * @param p the planning
    * @return a list of activities that is a critical path for the planning
    */
  def criticalPath(p:Planning):List[Activity] = {
    def PrecedingNode(j: Activity): Activity = {
      if (j.DefiningPredecessors.value isEmpty) null
      else p.ActivityArray(selectFrom(j.DefiningPredecessors.value))
      //random tie break, as it is likely that there will be few forks.
    }

    var CurrentActivity: Activity = PrecedingNode(p.SentinelActivity)
    var TaskList:List[Activity] = List.empty

    while (CurrentActivity != null) {
      TaskList = CurrentActivity :: TaskList
      CurrentActivity = PrecedingNode(CurrentActivity)
    }

    TaskList
  }

  /** returns the non-solid fragments of a ctitical path.
    *
    * @param p the planning including all tasks
    * @return a list of pair of activities. They are a subset of a citical path
    *         such that there is a tight additional dependency between them
    */
  def nonSolidCriticalPath(p:Planning):List[(Activity,Activity)] = {

    def PrecedingNode(j: Activity): Activity = {
      if (j.DefiningPredecessors.value isEmpty) null
      else p.ActivityArray(selectFrom(j.DefiningPredecessors.value))
      //random tie break, as it is likely that there will be few forks.
    }

    var CurrentActivity: Activity = PrecedingNode(p.SentinelActivity)
    var toreturn: List[(Activity, Activity)] = List.empty
    while (CurrentActivity != null) {
      val Predecessor = PrecedingNode(CurrentActivity)
      if (Predecessor != null && CurrentActivity.AdditionalPredecessors.value.contains(Predecessor.ID)) {
        toreturn = (Predecessor, CurrentActivity) :: toreturn
      }
      CurrentActivity = Predecessor
    }
    toreturn
  }

}

package oscar.cbls.scheduling

import oscar.cbls.search.SearchEngine

/**
 * finds a critical path for the given planning
 *
 */
object CriticalPathFinder extends SearchEngine{

  def apply(p:Planning):List[Activity] = {
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
}

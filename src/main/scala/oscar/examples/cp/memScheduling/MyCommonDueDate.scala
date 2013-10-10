package oscar.examples.cp.memScheduling

import oscar.cp.dsl.instances.CommonDueDateInstance

object MyCommonDueDate extends CommonDueDateInstance("data/memScheduling/common-due-date/sch1000/cdd1000_8.txt") with App {

  // Testing parsed values
  println(nbJobs)
  for (j <- jobs) {
    print(jobs(j) + " " + processingTimes(j) + " " + earliness(j) + " " + tardiness(j))
    println
  }
  println(dueDate)
  
  // TODO: Well... you know what to do, right? :)
  
}
package oscar.examples.cp.memScheduling

import oscar.cp.dsl.instances.SemiconductorTestingInstance

object MySemiconductorTesting extends SemiconductorTestingInstance("data/memScheduling/semiconductor-testing/semi305/semi305_1.txt") with App {

  // Testing parsed values
  println(nbOperations + " " + nbLots + " " + nbMachines)
  for (o <- operations) {
    println(operations(o) + " " + operationNumbers(o) + " " + lotNumbers(o) + " " +	predecessors(o) + " " + jobOperationTime(o) + " " +	jobDueDate(o) + " " + requiredMachines(o))
  }
  for (a <- setupTimeArcs) {
    println(setupTimeArcs(a) + " " + i(a) + " " + j(a) + " " + setupTimes(a))
  }
  
}
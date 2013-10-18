package oscar.cp.dsl.instances
import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class HybridReentrantSSIntance(filepath: String) extends InstanceReader(filepath) {
  
  val Array(nbJobs, nbMachines) = readLine asInt
  
  val operationsValues = nbJobs		// Operations are described by nbJobs values: 1 for each job.
  
  // T'avais mis nbJobs en nombre de ligne, t'es ouf, ya 10 jobs, il n'y a que trois lignes!
  // Les lignes ne décrivent pas les jobs mais les 3 opérations de chaque job! 
  // Les jobs c'est le numéro de la colonne en fait, la ligne c'est l'opération!
  
  val nbOperations 	= 3		// Each job is divided in 3 operations, see the definition of the instance files.
  												// Unfortunately, this cannot be found out from the instance file itself.
  												// Well, it could be feasible though, but it would require reading until the end of the file,
  												// thus dropping every line, then returning to where we were on the saved/rebuilt iterator.
  
  val firstOpTime 	= readLine asIntArrayFillerOfLength nbJobs
  val secondOpTime 	= readLine asInt
  val thirdOpTime 	= readLine asInt
  
  val cp = CPScheduler(firstOpTime.sum + secondOpTime.sum + thirdOpTime.sum)

}
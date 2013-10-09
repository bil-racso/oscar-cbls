package oscar.cp.dsl.instances

import oscar.cp.modeling.CPScheduler
import oscar.cp.dsl.InstanceReader

class AircraftLandingInstance(filepath: String) {
 
  val reader = new InstanceReader(filepath)
  
  val Array(nbPlanes, freezeTime) = reader.readLine
  val datas = reader.readDatas(16, 3)
  
  val Array(planes,
      			appearanceTimes, 
      			earliestLandingTimes, 
      			targetLandingTimes, 
      			latestLandingTimes, 
      			costsForEarlyLanding, 
      			costsForLateLanding
      			) = datas.slice(0, 7)
  
  val separationTimes = datas.slice(7, datas.length) 
  
  val cp = CPScheduler(latestLandingTimes.sum)

}
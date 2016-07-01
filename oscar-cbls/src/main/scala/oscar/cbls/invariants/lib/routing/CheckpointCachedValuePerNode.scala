package oscar.cbls.invariants.lib.routing

/**
 * Created by rdl on 24-06-16.
 */
class CachedValuePerNode[T](f:Int=>T,maxValue:Int) {

  var currentCheckpoint:Int = Int.MaxValue

  val cachedValues:Array[(Int,T)] = Array.fill(maxValue+1)(null)
  def setCheckpoint(checkpoint:Int){
    currentCheckpoint = checkpoint
  }

  def getValueAtCheckpoint(value:Int,checkpoint:Int):T = {
    if(checkpoint != currentCheckpoint)
      f(value)
    else {
      val cached = cachedValues(value)
      if (cached == null || cached._1 != checkpoint) {
        val newValue = f(value)
        cachedValues(value) = (checkpoint, newValue)
        newValue
      }else{
        cached._2
      }
    }
  }
}

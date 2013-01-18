package oscar.cbls.invariants.core.propagation
import collection.immutable.SortedMap

trait DistributedStorageUtility{

  var storage:SortedMap[Int, AnyRef] = SortedMap.empty

  /**returns null if nothing was stored*/
  final def getStorageAt[T](index:Int,default:T=null)=
    storage.getOrElse(index,default).asInstanceOf[T]

  final def storeAt(index:Int,value:AnyRef){
    storage = storage + ((index,value))
  }
}

trait StorageUtilityManager{

  var NextStoragePlace:Int = 0

  def getStorageIndex:Int = {
    val toreturn = NextStoragePlace
    NextStoragePlace+=1
    toreturn
  }

}

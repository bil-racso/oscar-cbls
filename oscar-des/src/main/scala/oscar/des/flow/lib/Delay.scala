package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.core.ItemClassHelper.ItemClass
import oscar.des.flow.core.{Fetchable, Putable}

import scala.collection.immutable.SortedMap

/**
 * this is a delay that can be added on a put.
 * so the proces takes an extra time putting to the sorage, or fetching from it.
 */
class Delay(s:Storage,m:Model,delay:Double,val id:Int) extends Putable with Fetchable{
  /**
   * put the amount of goods into the putable.
   * This is potentially blocking
   * @param amount: the items to be put in the puteable
   * @param block
   */
  override def put(amount: Int, i: ItemClass)(block: () => Unit){
    m.wait(delay){
      s.put(amount,i)(block)
    }
  }

  /**
   * fetch the amount of goods from the putable.
   * This is potentially blocking
   * @param amount
   * @param block the block to execute once the items are actually fetched. these are bigen to the block method
   */
  override def fetch(amount: ItemClass)(block: (ItemClass) => Unit): Unit = {
    m.wait(delay){
      s.fetch(amount)(block)
    }
  }

  def cloneReset(newModel:Model,storages:SortedMap[Storage,Storage]):Delay = {
    new Delay(storages(s),newModel,delay,id)
  }
}

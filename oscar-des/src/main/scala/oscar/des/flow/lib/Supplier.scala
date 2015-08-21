package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.core.{Inputter, StockNotificationTarget}
import oscar.des.flow.core.ItemClassHelper._
/**
 * represents a supplier. the main operation is order
 * @param m the model of the simulation
 * @param supplierDelay the delay of the supplier (random function)
 * @param deliveredPercentage the delivered percentage, when an order is placed
 * @param name the name of the supplier, for pretty printing purpose
 * @param verbose true to print order deliveries on the console
 * @author renaud.delandtsheer@cetic.be
 * */
class PartSupplier(m:Model,
                   supplierDelay:()=>Int,
                   deliveredPercentage:() => Int,
                   val name:String,
                   deliveredItemClass:ItemClass,
                   verbose:Boolean = true) {
  private var placedOrders = 0
  private var totalOrderedParts = 0
  private var deliveredOrders = 0
  private var totalDeliveredParts = 0

  def order[Content<:StockContentType](orderQuantity:Int, to:Storage[Content]): Unit ={
    totalOrderedParts += orderQuantity
    placedOrders += 1
    val willBeDelivered = (deliveredPercentage() * orderQuantity) / 100
    m.wait(supplierDelay()){
      if (verbose) println(name + ": delivered " + willBeDelivered + " parts to stock " + to.name +
        (if (willBeDelivered != orderQuantity) " (ordered: " + orderQuantity + ")" else ""))
      to.put(willBeDelivered,deliveredItemClass)(() => {totalDeliveredParts += willBeDelivered; deliveredOrders +=1})
    }
  }

  override def toString: String = name + " " + this.getClass.getSimpleName +
    ":: receivedOrders:" + placedOrders +
    " totalOrderedParts:" + totalOrderedParts +
    " deliveredOrders:" + deliveredOrders +
    " totalDeliveredParts:" + totalDeliveredParts
}


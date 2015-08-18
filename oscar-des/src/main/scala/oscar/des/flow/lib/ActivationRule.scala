package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.core.StockNotificationTarget

abstract class ActivationRule(a:Activable){
  a.setUnderControl()
  def activate(intensity:Int) {a.activate(intensity)}
}

class RegularActivation(m:Model, intensity:Int, tick:Float, a:Activable) extends ActivationRule(a:Activable){
  def doActivate(){
    activate(intensity)
    m.wait(tick){doActivate()}
  }
}

/**
 * This policy fills in a stock when it is below some threshold by placing an order to a supplier.
 * The storage will be refurbished when the supplier actually delivers the order
 *
 * @param s the storage that is refurbished through this policy
 * @param threshold the order is placed as soon as the stock gets below this threshold
 * @param verbose true to print order placement on the console
 * @param name a name used for pretty printing
 * @author renaud.delandtsheer@cetic.be
 * */
class OnStockThreshold(s:Storage,
                       m:Model,
                       a:Activable,
                       threshold:Int,
                       activationSize:Int=>Int,
                       verbose:Boolean = true,
                       period:Float, //set to zero if no tick needed
                       name:String)
  extends ActivationRule(a:Activable) with StockNotificationTarget{
  s.registerNotificationTarget(this)

  private var placedOrders = 0

  private var lastNotifiedlevel:Int = s.contentSize

  def notifyStockLevel(level: Int): Unit = {
    if(level <= threshold && lastNotifiedlevel > threshold){
      performOrder()
    }
    lastNotifiedlevel = level
  }

  private def doPerformOrder(): Unit ={
    val activation = activationSize(s.contentSize)
    if (verbose) println("threshold (" + threshold + ") reached on " + s.name + " (now:" + s.contentSize + "), activation " + activation)
    activate(activation)
    placedOrders += 1
  }

  protected def performOrder(): Unit ={
    if(period == 0){
      doPerformOrder
    }else{
      m.wait(period - (m.clock() % period)) {if (s.contentSize < threshold) doPerformOrder()}
    }
  }

  override def toString: String = name + " " + this.getClass.getSimpleName + ":: placedOrders:" + placedOrders
}


class OnOrder(s:Storage,
                       m:Model,
                       orderBook:Storage[Orders],
                       threshold:Int,
                       activationSize:Int=>Int,
                       verbose:Boolean = true,
                       period:Float, //set to zero if no tick needed
                       name:String) extends ActivationRule(a:Activable)

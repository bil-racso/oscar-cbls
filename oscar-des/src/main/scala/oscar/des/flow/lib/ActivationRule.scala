package oscar.des.flow.lib

import oscar.des.engine.Model
import oscar.des.flow.core.StockNotificationTarget

/**
 * a rule that activates a, based on its specific activation scheme
 * @param a the activeable that is activated by this activation
 */
abstract class ActivationRule(a:Activable){
  a.setUnderControl()
  def activate(intensity:Int) {a.activate(intensity)}
}

/**
 * activates "a" with intensity "intensity" every "delay". the initial activation is performed after "offset"
 * @param m the model of the simulation
 * @param intensity the intensity if the activation
 * @param delay the delay between consecutive activations
 * @param initialDelay the initial delay before the first activation
 * @param a the activeable that is activated by this activation
 */
class RegularActivation(m:Model, intensity:Int, delay:Float, initialDelay:Float, a:Activable) extends ActivationRule(a:Activable){
  def doActivate(){
    activate(intensity)
    m.wait(delay){doActivate()}
  }
  m.wait(initialDelay){doActivate()}
}

/**
 * This rule activates the activeable "a" by intensity activationSize(s.content)
 * whenever s.content goes below "threshold"
 *
 * if period is specified, it only perform the activation when time is a multiple of period.
 * the intensity is computed at the time of activation of "a"
 *
 * @param s the stock that is monitored by this rule
 * @param m tye model of the simulation
 * @param a the activeable that is activated by this activation
 * @param threshold the threshold for activation
 * @param activationSize a function that computes the level of activation, given the s.content
 * @param verbose true to have verbosities o nthe standard output
 * @param period the period of activation, set to zero for immediate activation
 * @param name the name of this rule, for debugging purposes
 */
class OnLowerThreshold(s:Storage,
                       m:Model,
                       a:Activable,
                       threshold:Int,
                       activationSize:Int=>Int,
                       verbose:Boolean = true,
                       period:Float,
                       name:String)
  extends ActivationRule(a:Activable) with StockNotificationTarget{
  s.registerNotificationTarget(this)

  private var placedOrders = 0

  private var lastNotifiedlevel:Int = s.contentSize

  override def notifyStockLevel(level: Int): Unit = {
    if(level <= threshold && lastNotifiedlevel > threshold){
      activation()
    }
    lastNotifiedlevel = level
  }

  private def doActivate(): Unit ={
    val activation = activationSize(s.contentSize)
    if (verbose) println("threshold (" + threshold + ") reached on " + s.name + " (now:" + s.contentSize + "), activation " + activation)
    activate(activation)
    placedOrders += 1
  }

  protected def activation(): Unit ={
    if(period == 0){
      doActivate
    }else{
      m.wait(period - (m.clock() % period)) {if (s.contentSize < threshold) doActivate()}
    }
  }

  override def toString: String = name + " " + this.getClass.getSimpleName + ":: placedOrders:" + placedOrders
}

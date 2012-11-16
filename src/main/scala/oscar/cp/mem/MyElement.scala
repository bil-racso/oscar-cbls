package oscar.cp.mem

import oscar.cp.core._
import oscar.reversible.ReversibleInt
import scala.collection.mutable.Map
import oscar.cp.constraints.Eq
import oscar.cp.modeling.CPSolver

/**
 * MyElement
 *
 *  Full Arc-Consistent Element Constraint.
 *
 *  @author Renaud Hartert - ren.hartert@gmail.com
 */

class MyElement(cp: Store, tab: Array[CPVarInt], x: CPVarInt, z: CPVarInt) extends Constraint(cp, "MyElement") {

  private val FAIL = CPOutcome.Failure
  private val SUSPEND = CPOutcome.Suspend
  private val SUCCESS = CPOutcome.Success

  private val supports: Map[Int, ReversibleInt] = Map()
  private val interSize: Map[Int, ReversibleInt] = Map()

  var on = true

  override def setup(l: CPPropagStrength): CPOutcome = {
    
    setIdempotent()
    
    if (adjustX() == FAIL) FAIL
    else {
      val out = propagate()
      if (out == FAIL || out == SUCCESS) out
      else {
        if (false) {
        x.callValRemoveWhenValueIsRemoved(this)
        z.callValRemoveWhenValueIsRemoved(this)
        for (i <- x.min to x.max; if x hasValue i) {
          tab(i).callValRemoveIdxWhenValueIsRemoved(this, i)
        }
        }else{
        x.callPropagateWhenDomainChanges(this)
        z.callPropagateWhenDomainChanges(this)
        for (i <- x.min to x.max; if x hasValue i) {
          tab(i).callPropagateWhenDomainChanges(this)         
        }}
        SUSPEND
      }
    }
  }

  override def propagate(): CPOutcome = {

    clearData()
    initData()

    for (i <- x.min to x.max; if x hasValue i) {
      if (!interSize.contains(i) || interSize(i).value == 0) {
        val out = removeValFromX(i)
        if (out == FAIL || out == SUCCESS) return out
      }
    }

    for (v <- z.min to z.max; if z hasValue v) {
      if (!supports.contains(v) || supports(v).value == 0) {
        if (z.removeValue(v) == FAIL) return FAIL
      }
    }
    
    if (x.isBound) bindX()
    else SUSPEND
  }

  override def valRemoveIdx(cpvar: CPVarInt, i: Int, v: Int): CPOutcome = {
    if (on) removeFromTab(i, v) else SUCCESS
  }

  override def valRemove(cpvar: CPVarInt, v: Int): CPOutcome = {
    if (on) {
      if (cpvar == x) removeFromX(v)
      else removeFromZ(v)
    } else SUCCESS
  }

  // Number of supports for a given value in z 
  private def initData() {
    for (i <- x.min to x.max; if x hasValue i) {
      for (v <- tab(i).min to tab(i).max; if tab(i) hasValue v) {
        if (z hasValue v) {
          if (supports contains v) supports(v).incr()
          else supports += (v -> new ReversibleInt(cp, 1))
          if (interSize contains i) interSize(i).incr()
          else interSize += (i -> new ReversibleInt(cp, 1))
        }
      }
    }
  }

  private def clearData() {
    for ((k, v) <- supports) v.setValue(0)
    for ((k, v) <- interSize) v.setValue(0)
  }

  // Reduces the number of supports of the value v
  private def reduceSupports(v: Int): CPOutcome = {
    if (supports contains v) {
      val c = supports(v)
      c.decr()
      if (c.value == 0) z.removeValue(v)
      else SUSPEND
    } else SUSPEND
  }

  // Reduces the number of common values between tab(i) and z
  private def reduceInterSize(i: Int): CPOutcome = {
    val c = interSize(i)
    c.decr()
    if (c.value == 0) removeValFromX(i)
    else SUSPEND
  }

  // Pruning when the value v of z is removed
  private def removeFromZ(v: Int): CPOutcome = {
    println("RemoveFromZ : v=" + v)
    // No more supports
    supports(v) setValue 0
    // Reduces intersection with relevant tab(i)
    for (i <- x.min to x.max; if x hasValue i; if tab(i) hasValue v) {
      val out = reduceInterSize(i)      
      if (out == FAIL || out == SUCCESS) return SUCCESS
    }
    SUSPEND
  }

  // Pruning when the value i of x is removed
  private def removeFromX(i: Int): CPOutcome = {
    println("RemoveFromX : i=" + i)
    if (x.isBound) bindX()
    else {
      // No more intersection
      interSize(i) setValue 0
      // Removes supports of z
      for (v <- tab(i).min to tab(i).max; if tab(i) hasValue v; if z hasValue v) {
        if (reduceSupports(v) == FAIL) return FAIL
      }
      SUSPEND
    }
  }

  // Pruning when the value v of tab(i) is removed
  private def removeFromTab(i: Int, v: Int): CPOutcome = {
    println("RemoveFromTab : i=" + i + ", v=" + v)
    if (x.hasValue(i) && z.hasValue(v)) {
      if (reduceSupports(v) == FAIL) FAIL
      else reduceInterSize(i)
    } else SUSPEND
  }

  // Removes the value i from the domain of x. If x is bound the constraint is replaced by an Equality constraint
  private def removeValFromX(i: Int): CPOutcome = {
    if (x.removeValue(i) == FAIL) FAIL
    else if (x.isBound) bindX()
    else SUSPEND
  }

  // Replaces the constraint by an Equality constraint
  private def bindX(): CPOutcome = {
    if (cp.post(new Eq(tab(x.value), z)) == FAIL) FAIL
    else {
      on = false
      SUCCESS
    }
  }

  // Remove inconsistent values of x (values are indexes of tab)
  private def adjustX(): CPOutcome = {
    if (x.updateMin(0) == FAIL) FAIL
    else if (x.updateMax(tab.size - 1) == FAIL) FAIL
    else if (x.isBound) bindX()
    else SUSPEND
  }
}

object MyElement {

  def apply(t: Array[CPVarInt], x: CPVarInt): CPVarInt = {

    val cp = x.store
    val max = t.map(_.max).max
    val min = t.map(_.min).min

    val z = CPVarInt(cp, min to max)
    cp.add(new MyElement(cp, t, x, z))
    z
  }

  def apply(t: Array[CPVarInt], x: CPVarInt, z: CPVarInt) = new MyElement(x.store, t, x, z)

  def main(args: Array[String]) = {

    test1()
    test2()
    test3()
    test4()
    test5()
    test6()
    test7()
    test8()
    test9()
    test10()
    test11()
  }

  def test1() {
    println("Test 1")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 10)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 4), CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3), CPVarInt(cp, Array(5, 11)))

    cp.add(MyElement(tab, x, z))

    cp.add(tab(3) != 5)

    assert(z.min == 1)
    assert(z.max == 4)

    assert(x.min == 0)
    assert(x.max == 2)
  }

  def test2() {
    println("Test 2")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 10)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 4), CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3), CPVarInt(cp, Array(5, 11)))

    cp.add(MyElement(tab, x, z))

    cp.add(x == 1)

    assert(z.min == 1)
    assert(z.max == 2)
  }

  def test3() {
    println("Test 3")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 10)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 4), CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3), CPVarInt(cp, Array(5, 11)))

    cp.add(MyElement(tab, x, z))

    cp.add(z != 1)

    assert(x.min == 0)
    assert(x.max == 3)

    cp.add(z != 2)

    assert(x.min == 0)
    assert(x.max == 3)
    assert(!x.hasValue(1))
  }

  def test4() {
    println("Test 4")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 10)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 4), CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3), CPVarInt(cp, 11))

    cp.add(MyElement(tab, x, z))

    assert(x.min == 0)
    assert(x.max == 2)
  }

  def test5() {
    println("Test 5")

    val cp = CPSolver()

    val z = CPVarInt(cp, 2)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3))

    cp.add(MyElement(tab, x, z))

    assert(x.min == 0)
    assert(x.max == 1)

    cp.add(tab(0) != 2)

    assert(x.isBound)
  }

  def test6() {
    println("Test 6")

    val cp = CPSolver()

    val z = CPVarInt(cp, 2)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3))

    cp.add(MyElement(tab, x, z))

    assert(x.min == 0)
    assert(x.max == 1)

    cp.add(x != 1)

    assert(x.isBound)
    assert(tab(0).value == 2)
  }

  def test7() {
    println("Test 7")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1, 2)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3))

    cp.add(MyElement(tab, x, z))

    assert(x.min == 0)
    assert(x.max == 1)

    cp.add(z != 2)

    assert(x.isBound)
    assert(tab(1).value == 2)
  }

  def test8() {
    println("Test 8")

    val cp = CPSolver()

    val z = CPVarInt(cp, 0 to 2)
    val x = CPVarInt(cp, 0 to 2)
    val tab = Array(CPVarInt(cp, 0 to 2), CPVarInt(cp, 0 to 2), CPVarInt(cp, 0 to 2))

    cp.add(MyElement(tab, x, z))

    assert(x.min == 0)
    assert(x.max == 2)

    cp.add(tab(0) != 0)
    cp.add(tab(1) != 1)
    cp.add(tab(2) != 2)

    assert(x.min == 0)
    assert(x.max == 2)

    cp.add(tab(0) == 1)
    cp.add(tab(2) != 1)

    assert(x.min == 0)
    assert(x.max == 2)

    cp.add(z != 1)

    assert(x.min == 1)
    assert(x.max == 2)

    cp.add(z != 2)

    assert(x.min == 1)
    assert(x.max == 2)

    cp.add(tab(1) != 0)

    assert(x.isBound)
  }
  
  def test9() {
    println("Test 9")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 3)
    val x = CPVarInt(cp, 0 to 3)
    val tab = Array(CPVarInt(cp, 1 to 2), CPVarInt(cp, 2 to 3), CPVarInt(cp, 2 to 3))

    cp.add(MyElement(tab, x, z))

    assert(x.min == 0)
    assert(x.max == 2)
    
    cp.add(x != 2)
    
    assert(x.min == 0)
    assert(x.max == 1)
    assert(z.min == 1)
    assert(z.max == 3)
    
    cp.add(tab(2) != 3)
    
    assert(x.min == 0)
    assert(x.max == 1)
    assert(z.min == 1)
    assert(z.max == 3)
  }
  
  def test10() {
    println("Test 10")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 3)
    val x = CPVarInt(cp, 0 to 1)
    val tab = Array(CPVarInt(cp, 1 to 2), CPVarInt(cp, 3))

    cp.add(MyElement(tab, x, z))
    
    assert(x.min == 0)
    assert(x.max == 1)
    assert(z.min == 1)
    assert(z.max == 3)
    
    cp.add(tab(0) != 1)
    
    assert(x.min == 0)
    assert(x.max == 1)
  }
  
  def test11() {
    println("Test 11")

    val cp = CPSolver()

    val z = CPVarInt(cp, 1 to 10)
    val x = CPVarInt(cp, 0 to 2)
    val tab = Array(CPVarInt(cp, 1 to 10), CPVarInt(cp, 2 to 10), CPVarInt(cp, 1))

    cp.add(MyElement(tab, x, z))
    
    cp.add(z == 1)
  }
}

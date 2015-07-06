package oscar.cp.core.watcher

import oscar.algo.reversible.TrailEntry
import oscar.cp.core.CPStore
import oscar.cp.core.Constraint

class WatcherListL2(store: CPStore) {
  
  private[this] var lastMagic = -1L
  private[this] var stack: Array[Constraint] = new Array[Constraint](4)
  private[this] var watchers: Array[Watcher] = new Array[Watcher](4)
  private[this] var index: Int = 0

  @inline final def length: Int = index
  
  @inline final def isEmpty = index == 0

  @inline final def register(constraint: Constraint, watcher: Watcher = null): Unit = {
    if (index == stack.length) growStack()
    stack(index) = constraint
    watchers(index) = watcher
    trail()
    index += 1
  }
  
  @inline final def clear(): Unit = {
    trail()
    index = 0
  }
  
  @inline final def enqueue(): Unit = {
    var i = index
    while (i > 0) { 
      i -= 1
      val watcher = watchers(i)
      if (watcher == null || watcher.shouldEnqueue()) {
        store.enqueueL2(stack(i))
      }
    }
  }  
  
  @inline private def trail(): Unit = {
    val contextMagic = store.magic
    if (lastMagic != contextMagic) {
      lastMagic = contextMagic
      val newIndex = index
      store.trail(new TrailEntry { final override def restore(): Unit = index = newIndex })
    }
  }
  
  // Double the size of the stack
  @inline private def growStack(): Unit = {
    val newStack = new Array[Constraint](stack.length * 2)
    val newWatchers = new Array[Watcher](watchers.length * 2)
    System.arraycopy(stack, 0, newStack, 0, stack.length)
    System.arraycopy(watchers, 0, newWatchers, 0, watchers.length)
    stack = newStack
    watchers = newWatchers   
  }
}
package oscar.cp.core.watcher

abstract class Watcher { def shouldEnqueue(): Boolean = true }
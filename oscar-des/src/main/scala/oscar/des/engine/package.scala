package oscar.des


package object engine {
import org.scala_tools.time.Imports._
	implicit def now(implicit m: Model[_]): DateTime = m.clock()
    val weeks = 7 days
}
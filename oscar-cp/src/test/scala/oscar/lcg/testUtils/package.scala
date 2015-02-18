package oscar.lcg

package object testUtils {

  @inline private def assert(assertion: Boolean, message: => Any): Unit = {
    if (!assertion) throw new java.lang.AssertionError("assertion failed: " + message)
  }

  @inline private def assert(assertion: Boolean): Unit = {
    if (!assertion) throw new java.lang.AssertionError("assertion failed.")
  }

  /** An implicit class to provide test functionalities to CPIntVar */
  implicit class LCGIntervalVarTestOps(val variable: LCGIntervalVar) extends AnyVal {

    /** Asserts that `value` is contained in `intVar` */
    def shouldContain(value: Int): Unit = {
      assert(variable.contains(value), s"$value should be in variable")
    }

    /** Asserts that each values in `values` is contained in `intVar` */
    def shouldContain(values: Int*): Unit = values.foreach(shouldContain)

    /** Asserts that each values in `values` is contained in `intVar` */
    def shouldContain(values: Traversable[Int]): Unit = values.foreach(shouldContain)

    /** Asserts that each values in `values` is contained in `intVar` */
    def shouldContain(values: Array[Int]): Unit = values.foreach(shouldContain)

    /** Asserts that `value` is not contained in `intVar` */
    def shouldNotContain(value: Int): Unit = {
      assert(!variable.contains(value), s"$value should not be in variable")
    }

    /** Asserts that each values in `values` is not contained in `intVar` */
    def shouldNotContain(values: Int*): Unit = values.foreach(shouldNotContain)

    /** Asserts that each values in `values` is not contained in `intVar` */
    def shouldNotContain(values: Traversable[Int]): Unit = values.foreach(shouldNotContain)

    /** Asserts that each values in `values` is not contained in `intVar` */
    def shouldNotContain(values: Array[Int]): Unit = values.foreach(shouldNotContain)
    
    /** Asserts that the variable is assigned to `value` */
    def shouldBeAssignedTo(value: Int): Unit = {
      assert(variable.isAssignedTo(value))
    }
  }
}
package oscar.cp

/**
 * @author Renaud Hartert : ren.hartert@gmail.com
 */
package object scheduling {
	
	implicit def activityPrecedence2Constraint(ap : ActivityPrecedence) = ap.withDelay(0)
}

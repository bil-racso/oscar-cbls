

package oscar.cp.core


/**
 * Boolean variable: it is nothing else than a 0-1 integer variable. <br>
 * 1 is used for true, 0 for false.
 * @author Pierre Schaus pschaus@gmail.com
 */
class CPVarBool(st: Store) extends CPVarIntImpl(st,0,1) {


    	
	def this(s: Store, b: Boolean) = {
		this(s)
		if (b) assign(1)
		else assign(0)
	}
	
	
	override def toString() = {
		if (isBound) {
			if (value == 0)  "false" else "true";
		} else {
			"false,true";
		}
	}

    /**
     * @return  a constraint setting the boolean variable to true (1)
     */
    def constraintTrue() = new oscar.cp.constraints.Eq(this,1);
    

    /**
     * @return  a constraint setting the boolean variable to false (0)
     */
    def constraintFalse() = new oscar.cp.constraints.Eq(this,0);

    /**
     * @return true if the variable is bound and bound to value 1
     */
    def isTrue =  isBound && value == 1;
    

    /**
     * @return true if the variable is bound and bound to value 0
     */
    def isFalse =  isBound && value == 0;
    
    
	/**
	 * Logical or
	 */
	def or(y: CPVarBool): CPVarBool = {
		val b = new CPVarBool(s);
		s.post(new oscar.cp.constraints.Or(Array(this,y),b));
		return b;
	} 
	
	/**
	 * Logical and
	 */
	def and(y: CPVarBool): CPVarBool = {
		val res = this.plus(y);
		res.isEq(2);
	} 
	
	def not(): CPVarBool = {
		val not = new CPVarBool(s);
		s.post(new oscar.cp.constraints.Not(this,not));
		not;
	} 
	
	
	def implies(y: CPVarBool) =  {
		// return this.not().or(y);
		val V = new CPVarBool(s);
	    s.post(new oscar.cp.constraints.Implication(this, y, V));
		V;
	}


}


object CPVarBool {

   /**
    * Creates a new CP Boolean Variable
    */
   def apply(cp: Store): CPVarBool = {
    new CPVarBool(cp)
   }
   
   def apply(cp: Store,b: Boolean): CPVarBool = {
    new CPVarBool(cp,b)
   }
   
   def getArray(cp: Store,nb: Int) = Array.fill(nb)(CPVarBool(cp));
}  
  

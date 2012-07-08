/*******************************************************************************
 * This file is part of OscaR (Scala in OR).
 *  
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 * 
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/gpl-3.0.html
 ******************************************************************************/

package oscar.cp.core;


import oscar.cp.constraints.Eq;
import oscar.cp.constraints.Implication;
import oscar.cp.constraints.Or;
import oscar.cp.constraints.Sum;
import oscar.cp.constraints.Not;

/**
 * Boolean variable: it is nothing else than a 0-1 integer variable. <br>
 * 1 is used for true, 0 for false.
 * @author Pierre Schaus pschaus@gmail.com
 */
public class CPVarBool extends CPVarInt{

    /**
     *
     * @param n > 0
     * @return an array of n fresh boolean variables
     */
    public static CPVarBool[] getArray(Store s, int n) {
        assert(n > 0);
        CPVarBool [] res = new CPVarBool[n];
        for (int i = 0; i < n; i++) {
            res[i] = new CPVarBool(s);
        }
        return res;
    }

    
    public static CPVarBool[][] getArray(Store s, int n, int m) {
        assert(n > 0);
        assert(m > 0);
        CPVarBool [][] res = new CPVarBool[n][m];
        for (int i = 0; i < n; i++) {
        	for (int j = 0; j < m; j++) {
        		res[i][j] = new CPVarBool(s);
        	}
            
        }
        return res;
    }
    
    
	public CPVarBool(Store s) {
		super(s,0,1);
	}
	
	public CPVarBool(Store s,boolean b) {
		this(s);
		if (b) {
			assign(1);
		} else {
			assign(0);
		}
	}
	
	@Override
	public String toString() {
		if (isBound()) {
			return getValue() == 0 ? "false" : "true";
		} else {
			return "false,true";
		}
	}

    /**
     * @return  a constraint setting the boolean variable to true (1)
     */
    public Constraint constraintTrue() {
        return new Eq(this,1);
    }

    /**
     * @return  a constraint setting the boolean variable to false (0)
     */
    public Constraint constraintFalse() {
        return new Eq(this,0);
    }

    /**
     * @return true if the variable is bound and bound to value 1
     */
    public boolean isTrue() {
        return  isBound() && getValue() == 1;
    }

    /**
     * @return true if the variable is bound and bound to value 0
     */
    public boolean isFalse() {
        return  isBound() && getValue() == 0;
    }
    
    
	/**
	 * Logical or
	 */
	public CPVarBool or(CPVarBool y) {
		CPVarBool b = new CPVarBool(getStore());
		getStore().post(new Or(new CPVarBool[]{this,y},b));
		return b;
	} 
	
	/**
	 * Logical and
	 */
	public CPVarBool and(CPVarBool y) {
		CPVarInt res = this.plus(y);
		return res.isEq(2);
	} 
	
	public CPVarBool not() {
		CPVarBool not = new CPVarBool(getStore());
		getStore().post(new Not(this,not));
		return not;
	} 
	
	
	public CPVarBool implies(CPVarBool y) {
		//return this.not().or(y);
		
		CPVarBool V = new CPVarBool(getStore());
		getStore().post(new Implication(this, y, V));
		return V;
	}

	//--------------------methods for the scala wrapper--------------------
    
	/**
	 * Scala wrapper: x | y
	 */
	public CPVarBool $bar(CPVarBool y) {
		return this.or(y);
	} 
    
	/**
	 * Scala wrapper: x || y
	 */
	public CPVarBool $bar$bar(CPVarBool y) {
		return this.or(y);
	}  

	/**
	 * Scala wrapper: x & y
	 */
	public CPVarBool $amp(CPVarBool y) {
		return this.and(y);
	}
	
	/**
	 * Scala wrapper: x && y
	 */
	public CPVarBool $amp$amp(CPVarBool y) {
		return this.and(y);
	}
	
	/**
	 * Scala wrapper: x ==> y
	 */
	public CPVarBool $eq$eq$greater(CPVarBool y) {
		return this.implies(y);
	}
	
	/**
	 * Scala wrapper: x != y
	 */
	public Constraint $bang$eq(CPVarBool y) {
		return new Not(this,y);
	}





}


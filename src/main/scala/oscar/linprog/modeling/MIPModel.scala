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

package oscar.linprog.modeling


import oscar.linprog._
import oscar.algebra._


/**
 * @author Pierre Schaus pschaus@gmail.com  
 */
class MIPVar(mip: MIPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity) extends AbstractLPVar(mip,name,lbound,ubound,false) {

	  	def this(mip: MIPSolver, name: String, unbounded: Boolean) = {
	      this(mip,name)
	      this.unbounded = unbounded 
	    }
	  
		/**
		 * Defines an Integer variable in the MIP solver with the specified integer range domain 
		 */
		def this(mip : MIPSolver, name : String,  domain : Range) = {
			this(mip, name, domain.min, domain.max)
			this.integer = true
		}

		/**
		 * Set the variable as an integer one
		 */
		def setInteger() {
			this.integer = true
		}		
		
}
	
object MIPVar { 
	  def apply(mip: MIPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity): MIPVar = new MIPVar(mip,name,lbound,ubound) 
	  def apply(mip : MIPSolver, name : String,  domain : Range): MIPVar =  new MIPVar(mip,name,domain)
}

class MIPSolver(solverLib: LPSolverLib.Value = LPSolverLib.lp_solve) extends AbstractLPSolver() {

    val solver = solverLib match {
      case LPSolverLib.lp_solve => new LPSolve()
      case LPSolverLib.glpk => new GlpkMIP()
      case _ => new LPSolve()
    }

    override def setVarProperties() = {
      super.setVarProperties();
      for (x <- vars; if (x._2.isInteger)) {
        solver.setInteger(x._2.index)
      }
    }

        /* PIECEWISE LINEAR FUNCTION
     * limits = break points of the piecewise function (0 is the first)
     * rates  = rates of the different portions
     * Q      = the value of the abscissa
     * limits and rates are Vectors of identical sizes !
     */
  
    // Linear functions
    private def linpwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name:String): MIPVar = {
      println("***** Piecewise linear function : LINEAR ***********")
      val Z = MIPVar(this,"Z_"+name,0,Double.PositiveInfinity)
      add(Z == rates(0)*Q,Some("Z_"+name))
      Z
    }
        
    // Convex functions
    private def convpwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name:String): MIPVar = {
      val num = limits.size;
      println("***** Piecewise linear function : CONVEX ***********")
      val Z = MIPVar(this,"Z_"+name,0,Double.PositiveInfinity)
      /* Way proposed by AMPL : more variables !
       * =======================================
       */
      val X = createVarMap(0 until num)(n => MIPVar(this,"X_"+name+n,0,{if(n==0) limits(n) else (limits(n) - limits(n-1))}))
      for(n<-0 until num) this.add(X(n) <= {if(n==0) limits(n) else limits(n)-limits(n-1)},Some("Forcing constraint - "+name+n))
      this.add(sum(0 until num)(n => X(n)) == Q,Some(name+"_convex_Somme des X == Q"))
      this.add(sum(0 until num)(n => rates(n)*X(n)) == Z,Some(name+"_convex_Z must stick to curve"))
      
      /* Alternative way (equivalent) : less var
       * =======================================
       *   //c is a vector of size num.
       *   //val c = (rates.tail.zip(rates).zip(limits)).map{case ((m2,m1),b)=>b*(m1-m2)}.scanLeft(0.0)(_+_)
       *   var c = 0.0
       *   for(i<-0 until num) {
       *     if (i==0){
       *       theSolver.add(Z >= rates(i)*Q + c)
       *     }
       *     else {
       *       c = c + limits(i-1)*rates(i-1) - limits(i-1)*rates(i)
       *       theSolver.add(Z >= rates(i)*Q + c)
       *     }
       *   }
	   */
       Z
    } 
    
    // Non-convex functions 
    private def ncpwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name:String): MIPVar = {
      val num = limits.size 
      println("***** Piecewise linear function : NON-CONVEX *******")
      // If a limit is too big, it can generate serious numerical errors.
      if (!(0 until limits.size-1).forall(k => limits(k) <= 10000000.0)) {
        println("limits out of bounds => Risk of numerical errors.")
        println("Only Double.PositiveInfinity is accepted for the last key of limits.")
        throw new IllegalArgumentException("limits out of bounds => Risk of numerical errors.")
      }
      
      val Y = createVarMap(0 until num)(n => MIPVar(this,"Y_"+name+n,0 to 1))						// binary variables
      val X = createVarMap(0 until num)(n => MIPVar(this,"X_"+name+n,0,Double.PositiveInfinity)) 	// auxiliary variables
      val Z = MIPVar(this,"Z_"+name,0,Double.PositiveInfinity)
      
      // C terms of the linear pieces.
      val c = new Array[Double](num)
      
      // Constraints on x.
      // val c = (rates.tail.zip(rates).zip(limits)).map{case ((m2,m1),b)=>b*(m1-m2)}.scanLeft(0.0)(_+_)      
      for (i<-0 until num) {
        if (i==0) {
          c(i)  = 0.0
          add(0 <= X(i),Some(name+"_nc_lowboundX_"+i))
          add(X(i) <= limits(i)*Y(i),Some(name+"_nc_upboundX_"+i))
        }
        else {
          c(i) = c(i-1) + limits(i-1)*rates(i-1) - limits(i-1)*rates(i)
          add(limits(i-1)*Y(i) <= X(i),Some(name+"_nc_lowboundX_"+i))
          add({ if (limits(i)==Double.PositiveInfinity) 1000000.0*Y(i) else limits(i)*Y(i) } >= X(i),Some(name+"_nc_upboundX_"+i))  
        }
      }
            
      // Binary constraints, sum of the X, and fitting to the curve
      add(sum(0 until num)(i=>X(i)) == Q,Some(name+"_nc_sommeX=Q"))
      add(sum(0 until num)(i=>Y(i)) == 1,Some(name+"_nc_sommeY=1"))
      add(sum(0 until num)(i=>(rates(i)*X(i) + c(i) * Y(i))) <= Z,Some(name+"_nc_coller a la courbe"))
      Z
    } 
    
    /**
     * Piecewise linear function
     * @author: Pierre-Yves Gousenbourger
     */
    def pwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name: String): MIPVar = {
        if(limits.size != rates.size){
          throw new IllegalArgumentException("limits must be the same size as rates.")
        }
        
        if(limits.size == 1) {
          linpwf(limits,rates,Q,name)
        }
        else if((0 until (rates.size-1)).forall(k => rates(k) < rates(k+1))) {
          convpwf(limits,rates,Q,name)
        }
        else {
          /* Non-convex (Multiple choice method - source : paper Vielma, Ahmed, Nemhauser :
           * "Mixed Integer models for non-separable piecewise linear optimization :
           *  Unifying Framework and Extension"
           *  Creation of the variables !
           */
          ncpwf(limits,rates,Q,name)
        }
     }        

    


}
	
object MIPSolver { 
	 def apply(solverLib: LPSolverLib.Value = LPSolverLib.lp_solve): MIPSolver = new MIPSolver(solverLib) 
}

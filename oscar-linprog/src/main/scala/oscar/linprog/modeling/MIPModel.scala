/*******************************************************************************
 * OscaR is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *   
 * OscaR is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *   
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 ******************************************************************************/
package oscar.linprog.modeling


import oscar.linprog._
import oscar.algebra._


/**
 * @author Pierre Schaus pschaus@gmail.com  
 */
class MIPFloatVar(mip: MIPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity) extends AbstractLPFloatVar(mip,name,lbound,ubound,false) {

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
		
		/**
		 * Set the variable as a binary one
		 */
		def setBinary() {
			this.binary = true
		}
		
}

object MIPFloatVar { 
	  def apply(name : String)(implicit mip: MIPSolver) = new MIPFloatVar(mip,name,0,Double.PositiveInfinity) 
      def apply(name : String, lbound: Double, ubound: Double)(implicit mip: MIPSolver) = new MIPFloatVar(mip,name,lbound,ubound)  
	  def apply()(implicit mip: MIPSolver) = new MIPFloatVar(mip,"",0,Double.PositiveInfinity) 
      def apply(lbound: Double, ubound: Double)(implicit mip: MIPSolver) = new MIPFloatVar(mip,"",lbound,ubound)  
	  def apply(domain : Range)(implicit mip: MIPSolver): MIPFloatVar =  new MIPFloatVar(mip,"",domain)	    
      def apply(mip: MIPSolver, name : String, lbound: Double = 0.0, ubound: Double = Double.PositiveInfinity): MIPFloatVar = new MIPFloatVar(mip,name,lbound,ubound) 
}


class MIPIntVar(mip : MIPSolver, name : String,  domain : Range) extends MIPFloatVar(mip,name,domain.min,domain.max) {
		this.integer = true
		this.binary = (domain.min == 0 && domain.max == 1)
		
	 def branchPriority()(implicit mip: MIPSolverGurobi) = {
		  //mip.solver.branchPriority()
		  //var.get(GRB.DoubleAttr.LB)
		  //BranchPriority
	 }

  
		
}

object MIPIntVar { 
	  def apply(name : String,  domain : Range)(implicit mip: MIPSolver): MIPIntVar =  new MIPIntVar(mip,name,domain)
	  def apply(domain : Range)(implicit mip: MIPSolver): MIPFloatVar =  new MIPIntVar(mip,"",domain)	  
	  def apply(mip : MIPSolver, name : String,  domain : Range): MIPIntVar =  new MIPIntVar(mip,name,domain)
}


class MIPSolver(val solver: AbstractLP) extends AbstractLPSolver() {
/*
    val solver = solverLib match {
      case LPSolverLib.lp_solve => new LPSolve()
      case LPSolverLib.glpk => new GlpkMIP()
      case LPSolverLib.gurobi => new GurobiLP()
      case _ => new LPSolve()
    }
*/
    override def setVarProperties() = {
      super.setVarProperties();
      for (x <- vars) {
        if(x._2.isBinary) {
          solver.setBinary(x._2.index)
        } else if(x._2.isInteger) {
        	solver.setInteger(x._2.index)
      	}
      }
    }

    /* PIECEWISE LINEAR FUNCTION
     * limits = break points of the piecewise function (0 is the first)
     * rates  = rates of the different portions
     * Q      = the value of the abscissa
     * limits and rates are Vectors of identical sizes !
     */
  
    // Linear functions
    private def linpwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name:String): MIPFloatVar = {
      println("***** Piecewise linear function : LINEAR ***********")
      val Z = MIPFloatVar(this,"Z_"+name,0,Double.PositiveInfinity)
      add(Z == rates(0) * Q, s"Z_${name}")
      Z
    }
        
    // Convex functions
    private def convpwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name:String): MIPFloatVar = {
      val num = limits.size;
      println("***** Piecewise linear function : CONVEX ***********")
      val Z = MIPFloatVar(this,"Z_"+name,0,Double.PositiveInfinity)
      /* Way proposed by AMPL : more variables !
       * =======================================
       */
      val X = createVarMap(0 until num)(n => MIPFloatVar(this,"X_"+name+n,0,{if(n==0) limits(n) else (limits(n) - limits(n-1))}))
      for(n<-0 until num) this.add(X(n) <= {if(n==0) limits(n) else limits(n)-limits(n-1)}, s"Forcing constraint - ${name}${n}")
      this.add(sum(0 until num)(n => X(n)) == Q, s"${name}_convex_Somme des X == Q")
      this.add(sum(0 until num)(n => rates(n)*X(n)) == Z, s"${name}_convex_Z must stick to curve")
      
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
    private def ncpwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name:String): MIPFloatVar = {
      val num = limits.size 
      println("***** Piecewise linear function : NON-CONVEX *******")
      // If a limit is too big, it can generate serious numerical errors.
      if (!(0 until limits.size-1).forall(k => limits(k) <= 10000000.0)) {
        println("limits out of bounds => Risk of numerical errors.")
        println("Only Double.PositiveInfinity is accepted for the last key of limits.")
        throw new IllegalArgumentException("limits out of bounds => Risk of numerical errors.")
      }
      
      val Y = createVarMap(0 until num)(n => MIPIntVar(this,"Y_"+name+n,0 to 1))						// binary variables
      val X = createVarMap(0 until num)(n => MIPFloatVar(this,"X_"+name+n,0,Double.PositiveInfinity)) 	// auxiliary variables
      val Z = MIPFloatVar(this,"Z_"+name,0,Double.PositiveInfinity)
      
      // C terms of the linear pieces.
      val c = new Array[Double](num)
      
      // Constraints on x.
      // val c = (rates.tail.zip(rates).zip(limits)).map{case ((m2,m1),b)=>b*(m1-m2)}.scanLeft(0.0)(_+_)      
      for (i<-0 until num) {
        if (i==0) {
          c(i)  = 0.0
          add(0 <= X(i), s"${name}_nc_lowboundX_${i}")
          add(X(i) <= limits(i)*Y(i), "${name}_nc_upboundX_${i}")
        }
        else {
          c(i) = c(i-1) + limits(i-1)*rates(i-1) - limits(i-1)*rates(i)
          add(limits(i-1)*Y(i) <= X(i), s"${name}_nc_lowboundX_${i}")
          add({ if (limits(i)==Double.PositiveInfinity) 1000000.0*Y(i) else limits(i)*Y(i) } >= X(i), s"${name}_nc_upboundX_${i}")  
        }
      }
            
      // Binary constraints, sum of the X, and fitting to the curve
      add(sum(0 until num)(i=>X(i)) == Q, name+"_nc_sommeX=Q")
      add(sum(0 until num)(i=>Y(i)) == 1)// ,name+"_nc_sommeY=1"
      add(sum(0 until num)(i=>(rates(i)*X(i) + c(i) * Y(i))) <= Z,name+"_nc_coller a la courbe")
      Z
    } 
    
    /**
     * Piecewise linear function
     * @author Pierre-Yves Gousenbourger
     */
    def pwf(limits:IndexedSeq[Double],rates:IndexedSeq[Double],Q: LinearExpression, name: String): MIPFloatVar = {
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

case class MIPSolverLPSolve() extends MIPSolver(new LPSolve())
case class MIPSolverGLPK() extends MIPSolver(new GlpkMIP())
case class MIPSolverGurobi() extends MIPSolver(new GurobiLP())
	
abstract class MIPModelGLPK {
  implicit val mipsolver = new MIPSolverGLPK()
}
abstract class MIPModelLPSolve {
  implicit val mipsolver = new MIPSolverLPSolve()
}
abstract class MIPModelGurobi {
  implicit val mipsolver = new MIPSolverGurobi()
} 


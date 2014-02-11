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

import org.gnu.glpk._

/**
 * @author Pierre Schaus pschaus@gmail.com
 */
class GlpkMIP extends GlpkLP {
  
	override def solveModel(): LPStatus.Value = {
			//println("writing to lp")
    		//GLPK.glp_write_lp(lp,null,"model_glpk.lp")
    		val iocp = new glp_iocp()    
    		GLPK.glp_init_iocp(iocp)
    		
    		iocp.setPresolve(GLPKConstants.GLP_ON)
    
    		iocp.setFp_heur(GLPKConstants.GLP_ON)
    		iocp.setBr_tech(GLPKConstants.GLP_BR_MFV)
    		iocp.setGmi_cuts(GLPKConstants.GLP_ON)
    		iocp.setMir_cuts(GLPKConstants.GLP_ON)
    		iocp.setCov_cuts(GLPKConstants.GLP_ON)
    		iocp.setClq_cuts(GLPKConstants.GLP_ON)
    	
			
    		iocp.setMsg_lev(GLPKConstants.GLP_MSG_ERR)
    		if(timeout < Int.MaxValue) {
    			iocp.setTm_lim(timeout)
    		}

    		GLPK.glp_intopt(lp, iocp) match {
    		  case 0 => println("Solution process was successful")
    		  case GLPKConstants.GLP_EBOUND => println("Unable to start search. Some variables have incorrect bounds")
    		  case GLPKConstants.GLP_EROOT => println("Unable to start search. No optimal basis provided (turn on LP presolve)")
    		  case GLPKConstants.GLP_ENOPFS => println("No primal feasible solution")
    		  case GLPKConstants.GLP_EFAIL => println("Search terminated earlier due to solver failure")
    		  case GLPKConstants.GLP_EMIPGAP => println("Relative MIP Gap tolerance reached")
    		  case GLPKConstants.GLP_ETMLIM => println("Search reached its time limit")
    		  case GLPKConstants.GLP_ESTOP => println("Search was terminated from application")
    		}
    		
    		GLPK.glp_mip_status(lp)  match {	  
    		  case GLPKConstants.GLP_UNDEF => LPStatus.UNBOUNDED
    		  case GLPKConstants.GLP_NOFEAS => LPStatus.INFEASIBLE
    		  case GLPKConstants.GLP_OPT => {
    		  	sol = Array.tabulate(nbCols)(col => GLPK.glp_mip_col_val(lp,col +1))
    			objectiveValue = GLPK.glp_get_obj_val(lp)
    			LPStatus.OPTIMAL
    		  }
    		  case GLPKConstants.GLP_FEAS => {
    		  	sol = Array.tabulate(nbCols)(col => GLPK.glp_mip_col_val(lp,col +1))
    			objectiveValue = GLPK.glp_get_obj_val(lp)
    			LPStatus.SUBOPTIMAL
    		  } 
    	   } 	
    } 
}
	

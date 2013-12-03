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
			
    		//ioocp.setMip_gap(0.03) 


    		val ret = GLPK.glp_intopt(lp, iocp)
    		if (ret != 0) {
    		  println("glpk return code was not zero, must be problem somewhere...")
    		}
    		
    		GLPK.glp_mip_status(lp)  match {	  
    		  case GLPKConstants.GLP_UNBND => LPStatus.UNBOUNDED
    		  case GLPKConstants.GLP_INFEAS => LPStatus.INFEASIBLE
    		  case GLPKConstants.GLP_OPT => {
    		  	sol = Array.tabulate(nbCols)(col => GLPK.glp_mip_col_val(lp,col +1))
    			objectiveValue = GLPK.glp_get_obj_val(lp)
    			LPStatus.OPTIMAL
    		  }
    		  case _ => {
    		  	sol = Array.tabulate(nbCols)(col => GLPK.glp_mip_col_val(lp,col +1))
    			objectiveValue = GLPK.glp_get_obj_val(lp)
    			LPStatus.SUBOPTIMAL
    		  } 
    	   } 	
    } 
}
	

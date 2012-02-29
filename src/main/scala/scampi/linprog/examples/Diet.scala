/*******************************************************************************
 * This program and the accompanying materials
 * are made available under the terms of the GNU Lesser Public License v3
 * which accompanies this distribution, and is available at
 * http://www.gnu.org/licenses/lgpl.html
 * 
 * Contributors:
 *     www.n-side.com
 ******************************************************************************/
package scampi.linprog.examples

import scampi.linprog.modeling._

/**
 * The goal of the diet problem is to find the cheapest combination of foods 
 * that will satisfy all the daily nutritional requirements of a person. 
 * The problem is formulated as a linear program where the objective is to minimize cost and 
 * meet constraints which require that nutritional needs be satisfied. 
 * We include constraints that regulate the number of 
 * calories and amounts of vitamins, minerals, fats, sodium and cholesterol in the diet.
 */
object Diet extends LPModel {
	
  case class Nutriment(name: String) {
    val shortName = name
  }
  case class Food(x : LPVar, price: Double, contents: Nutriment => Double)

  def main(args: Array[String]) {


		val lp = new LPSolver()
	  
		val nutriments = List("A","C","B1","B2","NA","CAL").map{Nutriment}
		
		def nvar(name : String) = LPVar(lp,name,2,10) //Each food is limited between 2 and 10
		
		val foods = List(
				(nvar("Beef"),      3.19, List(60,20,10,15,938,295)),
				(nvar("Chicken"),   2.59, List(8,0,20,20,2180,770)),
				(nvar("Fish"),      2.29, List(8,10,15,10,945,440)),
				(nvar("Ham"),       2.89, List(40,40,35,10,278,430)),
				(nvar("Macaroni"),  1.89, List(15,35,15,15,1182,315)),
				(nvar("MeatLoaf"),  1.9, List(70,30,15,15,896,400)),
				(nvar("Spaghetti"), 1.99, List(25,50,25,15,1329,370)),
				(nvar("Turkey"),    2.49, List(60,20,15,10,1397,450))).map
				{case (n,p,nut) => Food(n,p,nutriments.zip(nut.map(_.toDouble)).toMap)}
		
		//minimize the total cost
		lp.minimize(sum(foods){f => f.price*f.x}) subjectTo {
			//for each nutriment, at least 700 must be present in the Diet
			for (n <- nutriments) {
				lp.add(sum(foods){f => f.contents(n)*f.x} >= 700)
			}		
		}
		
		println("objective: "+lp.getObjectiveValue())
		println("----------")
		println(foods.map(_.x).mkString("\n"))
		
		lp.release() // release memory
  }
}


  

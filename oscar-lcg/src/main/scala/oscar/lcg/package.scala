package oscar

import oscar.lcg.core.LCGStore

package object lcg {

  type IntVar = oscar.lcg.variables.IntVar
  final val IntVar = oscar.lcg.variables.IntVar
  
  trait LCGModel { implicit val lcgStore = new LCGStore }
}
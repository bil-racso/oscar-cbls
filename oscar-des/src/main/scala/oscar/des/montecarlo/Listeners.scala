package oscar.des.montecarlo

import oscar.des.flow.lib.DoubleExpr

//To estimate over different runs
//how to find names that are obviously statistics over different runs

class Statistics
//this only considers the latest valuee of e; at the end of the simulation run, and performs an average over several runs
class Mean(e:DoubleExpr) extends Statistics
class Variance(e:DoubleExpr) extends Statistics


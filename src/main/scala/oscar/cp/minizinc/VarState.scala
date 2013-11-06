package oscar.cp.minizinc

import oscar.cp.minizinc.FZType._

class VarState (
    val name: String,
    val output: Boolean,
    val array: Boolean,
    val first: Boolean,
    val last: Boolean,
    val size: Any,
    val tp: FZType){

}
package oscar.cp.minizinc

import scala.collection.mutable.HashMap
import FZType._

class MinizincModel {
	var dict = new HashMap[String, (FZType, FZObject)]
}

package oscar.reversible

class ReversibleSubsetIndexedArray(store: ReversibleSearchNode,min: Int, max: Int) {

    //s1: Set[Int], s2: Set[Int]
    //if (!s1.forall(s2.contains(_))) throw new RuntimeException("s1 must be included in s2")
  
  

  
  
    val size1 = new ReversibleInt(store,0) // subset initially empty
    val size2 = new ReversibleInt(store,max-min+1) // superset contains everything
    
  
    val values = Array.tabulate(size2.value)(i => i)
    val indexes = Array.tabulate(size2.value)(i => i)
    
    
    def requires(value: Int) {
		assert(checkVal(value));
		if (isRequired(value)) return;
		if (!isPossible(value)) throw new RuntimeException(value+" cannot be required since it is event not possible")
		exchangePositions(value, values(size1.value)+min);
		size1.incr()
		assert(size1.value <= values.length);
	}
    
    def excludes(value: Int) {
		assert(checkVal(value))
		if (!isPossible(value)) return // it is already not possible
		if (isRequired(value)) throw new RuntimeException(value+" is required so it cannot be excluded")
		exchangePositions(value, values(size2.value-1)+min);
		size2.decr()
		assert(size1.value <= values.length);
	}
      
	def exchangePositions(value1: Int, value2: Int) {
		assert(checkVal(value1));
		assert(checkVal(value2));
		val v1 = value1 - min;
		val v2 = value2 - min;
		val i1 = indexes(v1);
		val i2 = indexes(v2);
		values(i1) = v2;
		values(i2) = v1;
		indexes(v1) = i2;
		indexes(v2) = i1;
	}    
    
	def requiredSet: Set[Int] = {
	  (for (i <- 0 until size1.value) yield min+values(i)).toSet
	}
	
    def possibleSet: Set[Int] = {
	  (for (i <- 0 until size2.value) yield min+values(i)).toSet
	}
    
    
	def isRequired(value: Int): Boolean = {
		if (value < min || value > max) false;
		else indexes(value-min) < size1.value;
	}

	def isPossible(value: Int): Boolean = {
		if (value < min || value > max) false;
		else indexes(value-min) < size2.value;
	}	
	
    
    
    def checkVal(value: Int) = {
		assert(value >= min);
		assert(value <= max);
		true;
	}
    
 
}

object ReversibleSubsetIndexedArray {
  def apply(store: ReversibleSearchNode,min: Int, max: Int) = new ReversibleSubsetIndexedArray(store,min,max)
  def apply(store: ReversibleSearchNode,possible: Set[Int]) = {
    val (min,max) = (possible.min,possible.max)
    val res = new ReversibleSubsetIndexedArray(store,min,max)
    for (i <- min to max; if !possible.contains(i)) {
      res.excludes(i)
    }
    res
  }
  
}

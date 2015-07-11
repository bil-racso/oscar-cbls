package oscar.cp.premulative

object GraphAlgorithms {
  
    val ninf = Int.MinValue
    
  def makeMatrix(ntasks: Int, pred: List[Tuple3[Int,Int,Int]]): Array[Array[Int]] = {
    val tab = Array.tabulate(ntasks)(t => Array.tabulate(ntasks)(s => ninf))
    for((i,j,w) <- pred){
      tab(i)(j) = w
    }
    tab
  }
    def computeTransitiveClosure(tab: Array[Array[Int]]){
    //tab = getAdjacencyMatrix();
    val s = tab.length; 
    for(k <- 0 until s){
      for(i <- 0 until s){
        if(tab(i)(k)>ninf){
          for(j <- 0 until s){
            if(tab(k)(j)>ninf && tab(i)(j) < tab(i)(k)+tab(k)(j)){
              tab(i)(j) = tab(i)(k)+tab(k)(j);
            }
          }
        }
      }
    }
  }
  def copy(tab: Array[Array[Int]]): Array[Array[Int]] = {
    val res = Array.tabulate(tab.length)(t => Array.tabulate(tab.length)(s => 0))
    val s = tab.length; 
    for(i <- 0 until s){
      for(j <- 0 until s){
          res(i)(j) = tab(i)(j)
      }
    }
    res
  }

  def findCliques(tab: Array[Array[Int]]): List[List[Int]] = {
    var res = List.empty[List[Int]]
    val cnt = Array.tabulate(tab.length)(i => 0)
    
    var s = 0
    def inter(p: List[Int], v: Int): List[Int] = {
      p.filter(x => tab(x)(v)==1)
    }
    def bk(r: List[Int], p: List[Int], x: List[Int]): Boolean = {
      if(p.isEmpty && x.isEmpty){
        //println(r.length+" "+r)
        if(r.length > 1)res = r::res
        s = r.length
        for(i <- r){cnt(i)+=1}
        return true
      }else{
        var pp = p.sortBy(i => (cnt(i),-inter(p,i).length))
        var xx = x
        for(v <- p.sortBy(i => (cnt(i),-inter(p,i).length))){
          if(bk(v::r,inter(pp,v),inter(xx,v)))return true
          pp = pp.tail
          xx = (v :: xx).distinct
        }
        return false
      }
    }
    while(cnt.min<=0){
      val p = Array.tabulate(tab.length)(i=>i).toList.sortBy(i=>(cnt(i),-tab(i).sum))
      //println(cnt.toList)
      //println(tab.map(_.sum).toList)
      bk(List.empty[Int],p,List.empty[Int])
    }
    //res.foreach(println)
    print("("+res.length+")")
    //print(cnt.mkString(", "))
    
    //get rid of stupid cliques.
    return res.filter(_.length > 3)
  }
    
  def findAllCliques(tab: Array[Array[Int]]): List[List[Int]] = {
    var res = List.empty[List[Int]]
    var s = 0
    def inter(p: List[Int], v: Int): List[Int] = {
      p.filter(x => tab(x)(v)==1)
    }
    def bk(r: List[Int], p: List[Int], x: List[Int]){
     // if(res.length >10) return
      if(r.length+p.length < s)return
      if(p.isEmpty && x.isEmpty){
        println(r.length+" "+r)
        if(r.length > 1)res = r::res
        s = r.length
      }else{
        var pp = p//.sortBy(-inter(p,_).length)
        var xx = x
        for(v <- p/*.sortBy(-inter(p,_).length)*/){
          bk(v::r,inter(pp,v),inter(xx,v))
          pp = pp.tail
          xx = (v :: xx).distinct
        }
      }
    }
    bk(List.empty[Int],Array.tabulate(tab.length)(i=>i).toList,List.empty[Int])
    print("("+res.length+")")
    //println("found cliques: "+res.length)
    return res
  }
  
  
  def todot(tab: Array[Array[Int]], dur:Array[_ <: Any]){
    println("digraph{")
    for (t <- 0 until tab.length){
      println(t+" [label=\""+t+","+dur(t)+"\"]")
    }
    for(i <- 0 until tab.length; j <- 0 until tab.length){
      val w = tab(i)(j)
      if(w > ninf){
        println(i+"->"+j+" [label=\""+w+"\"]")
      }  
    }
    println("}")
  }
  
  def makeTransitiveReduction(tab: Array[Array[Int]]){
    val nb = Array.tabulate(tab.length)(t => Array.tabulate(tab.length)(s => 0))
     
    val s = tab.length; 
    for(k <- 0 until s){
      for(i <- 0 until s){
        if(tab(i)(k)>ninf){
          for(j <- 0 until s){
            if(tab(k)(j)>ninf && tab(i)(j) < tab(i)(k)+tab(k)(j)){
              tab(i)(j) = tab(i)(k)+tab(k)(j);
              nb(i)(j) = 0
            }
            if(tab(k)(j)>ninf && tab(i)(j)== tab(i)(k)+tab(k)(j)){
              nb(i)(j) += 1
            }
          }
        }
      }
    }
    for(i <- 0 until s){
      for(j <- 0 until s){
        if(nb(i)(j)>0){
          tab(i)(j) = ninf
        }
      }
    }
   /* 
    for(int k=0;k<s;k++){
      for(int i=0; i<s; i++){
        if(tab[i][k]>ninf){
          for(int j=0; j<s; j++){
            if(tab[k][j]>ninf && tab[i][j] < tab[i][k]+tab[k][j]){
              tab[i][j] = tab[i][k]+tab[k][j];
              nb[i,j] = 0;
            }
            if(tab[k][j]>ninf && tab[i][j]==tab[i][k]+tab[k][j])nb[i,j]++;
          }
        }
      }
    }
    
    forall(i in tab.rng(), j in tab.rng()){
      if(tab[i][j]>ninf && nb[i,j]>0){
        tab[i][j]=ninf;
      }
    }
    return tab;

*/  }

}
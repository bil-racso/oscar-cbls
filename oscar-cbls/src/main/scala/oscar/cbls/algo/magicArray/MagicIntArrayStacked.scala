package oscar.cbls.algo.magicArray

class MagicIntArrayStacked(maxLevel:Int, initVal:(Int => Int), size:Int) extends Iterable{

  private[this] val levelToArray:Array[Array[Int]] = Array.tabulate(maxLevel+1)(level => if(level == 0) Array.tabulate(size)(initVal) else Array.fill(size)(0))
  private[this] val levelToIsValueChangedAtNextLevel:Array[MagicBoolArray] = Array.tabulate(maxLevel)(level => MagicBoolArray(size,false))
  private[this] var currentLevel:Int = 0

  def update(indice:Int,value:Int){
    levelToArray(currentLevel)(indice) = value
    if(currentLevel!=0)levelToIsValueChangedAtNextLevel(currentLevel-1)(indice) = true
  }

  def apply(indice:Int):Int = {
    var attemptLevel = currentLevel
    while(attemptLevel>0){
      val levelBelow = attemptLevel - 1
      if(levelToIsValueChangedAtNextLevel(levelBelow)(indice)){
        return levelToArray(attemptLevel)(indice)
      }else{
        attemptLevel = levelBelow
      }
    }
    levelToArray(0)(indice)
  }

  def pushLevel(){
    require(currentLevel < maxLevel,"MagicIntArrayStacked was declaring with max " + maxLevel + " levels; trying to push more")
    levelToIsValueChangedAtNextLevel(currentLevel).all = false
    currentLevel += 1
  }

  def popLevel(dropChanges:Boolean){
    require(currentLevel > 0,"trying to pop level zero")
    if(dropChanges){
      currentLevel -= 1
    }else{
      val newLevel = currentLevel - 1
      for(changedID <- levelToIsValueChangedAtNextLevel(currentLevel)){
        levelToArray(newLevel)(changedID) = levelToArray(currentLevel)(changedID)
      }
      currentLevel = newLevel
    }
  }

  def cloneTopArray:Array[Int] = {
    Array.tabulate(size)(this(_))
  }

  override def iterator : Iterator[Int] = {
    cloneTopArray.iterator
  }

  override def toString : String = "MagicIntArrayStacked(size:" + size + " level:" + currentLevel + " " + cloneTopArray.mkString(",") + ")"
}

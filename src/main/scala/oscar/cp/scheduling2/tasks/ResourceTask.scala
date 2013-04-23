package oscar.cp.scheduling2.tasks

import oscar.cp.core.CPVarInt

abstract class ResourceTask {

  def start: CPVarInt
  
  def duration: CPVarInt
  
  def end: CPVarInt
  
  def height: CPVarInt
  
  
  
}
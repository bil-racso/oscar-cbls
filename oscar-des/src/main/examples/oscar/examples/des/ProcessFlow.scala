package oscar.des.flow

import oscar.des.engine.Model
import oscar.des.flow.lib._

object testBelt extends App with HelperForProcess{

  val m = new Model
  val verbose = true

  //a process that has two inputs, and one output (eg: soldering)
  println("start simulate...")

  val input = new Storage(200, 10, "input", verbose)
  val output = new Storage(22, 0, "output", verbose)
  val belt = new ConveyerBeltProcess(m, 5, 1, List((1, input)), List((1, output)), "belt", verbose)

  val slowlyFeedingInput  = new SingleBatchProcess(m, 29, List(), List((10, input)), "slowlyFeedingInput", verbose)

  m.simulate(100, verbose)

  println("done.")
  println()
  println(input)
  println(output)
  println(belt)
  println(slowlyFeedingInput)
}

object TestProcess extends App with HelperForProcess{

  val m = new Model
  val verbose = true

  //a process that has two inputs, and one output (eg: soldering)
  println("start simulate...")

  val stockA = new Storage(200, 100, "stockA", verbose)
  val stockB = new Storage(300, 300, "stockB", verbose)

  val supplierforA = new PartSupplier(m, 50, 50 ,"SupplierForA", verbose)
//  OrderOnStockTreshold(stockA, 30, _ => 100, supplierforA, verbose)
  val orderPolicy = new OrderOnStockThresholdWithTick(stockA, m, 30, 100, _ => 100, supplierforA, verbose, "order")

  val middleStock = new Storage(7, 0, "middleStock", verbose)
  val trash = new OverflowStorage(5, 0, "trash", verbose)

  val emptyingTrash = SingleBatchProcess(m, 29, List((2, trash)), List(), "emptyingTrash", verbose)

  val soldering = BatchProcess(m, 2, 13, List((1, stockA),(2, stockB)), List((1, middleStock),(1, trash)), "soldering", verbose)

  val outputStock = new Storage(50, 10, "outputStock", verbose)

  val attaching = SingleBatchProcess(m, 11, List((3, middleStock),(1, stockA)), List((1, outputStock)), "attaching", verbose)

  val delivering = SingleBatchProcess(m, 61, List((20, outputStock)), List(), "delivering", verbose)

  m.simulate(10000, verbose)

  println("done.")
  println()
  println(stockA)
  println(stockB)
  println(outputStock)
  println(trash)
  println(soldering)
  println(attaching)
  println(delivering)
  println(orderPolicy)
  println(supplierforA)
}

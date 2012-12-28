package oscar.util

import java.io.IOException
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source

class OutFile(filepath: String, critical: Boolean, verbous: Boolean) {

  val file: BufferedWriter = openFile()

  private def openFile(): BufferedWriter = {
    try {
      new BufferedWriter(new FileWriter(filepath))
    } catch {
      case e: Error => {
        errorHandling(e)
        null
      }
    }
  }

  private def errorHandling(e: Error) {
    if (verbous) println(e.getMessage)
    if (critical) System.exit(-1)
  }

  def write(line: String) = {
    try file.write(line)
    catch {
      case e: Error => errorHandling(e)
    }
  }

  def writeln(line: String) = write(line + "\n")

  def close() = {
    try file.close()
    catch {
      case e: Error => errorHandling(e)
    }
  }
}

object OutFile {
  def apply(filepath: String, critical: Boolean = true, verbous: Boolean = true) = new OutFile(filepath, critical, verbous)
}
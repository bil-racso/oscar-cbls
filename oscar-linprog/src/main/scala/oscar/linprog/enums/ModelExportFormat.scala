/*******************************************************************************
  * OscaR is free software: you can redistribute it and/or modify
  * it under the terms of the GNU Lesser General Public License as published by
  * the Free Software Foundation, either version 2.1 of the License, or
  * (at your option) any later version.
  *
  * OscaR is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU Lesser General Public License  for more details.
  *
  * You should have received a copy of the GNU Lesser General Public License along with OscaR.
  * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
  ******************************************************************************/

package oscar.linprog.enums

sealed abstract class ModelExportFormat(val extension: String) {
  /**
   * Returns true if the given [[java.nio.file.Path]] has the correct file extension
   */
  def checkExtension(filepath: java.nio.file.Path): Boolean = {
    import oscar.linprog._

    filepath.extension.equals(extension)
  }
}

case object LP extends ModelExportFormat("lp")
case object MPS extends ModelExportFormat("mps")

object ModelExportFormat {
  def formats: Seq[ModelExportFormat] = Seq(LP, MPS)
}
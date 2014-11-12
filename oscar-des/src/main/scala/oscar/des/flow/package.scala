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

package oscar.des

/**
 * This package proposes a set of modeling artifacts to represent and simulate workflows.
 * It includes representation of [[oscar.des.flow.Storage]] (as well as [[oscar.des.flow.OverflowStorage]], processes ([[oscar.des.flow.SingleBatchProcess]], [[oscar.des.flow.BatchProcess]], and [[oscar.des.flow.ConveyerBeltProcess]])
 * and ordering policies (such as [[oscar.des.flow.OrderOnStockTreshold]]) and [[oscar.des.flow.PartSupplier]].
 *
 * As many of these model can intake random functions,
 * the trait [[oscar.des.flow.HelperForProcess]] provides a set of implicit methods to convert constants into such functions,
 * to keep you scripts easy to read and write.
 * @author renaud.delandtsheer@cetic.be
 */
package object flow {

}

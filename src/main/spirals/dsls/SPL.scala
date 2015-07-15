/**
 * _______  _______  ___   __      ____     Automatic
 * / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 * _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 * /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 * of DSP Algorithms
 * https://bitbucket.org/GeorgOfenbeck/spirals
 * SpiralS 0.1 Prototype - ETH Zurich
 * Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ch.ethz.spirals.dsls


import org.apache.commons.math3.linear._
import org.apache.commons.math3.complex._

/**
 * SPL is the base type we operate on which is a always a special matrix in the context of e.g. DFTs such as
 * Stride permutation etc.
 * @param size
 */


sealed abstract class SPL(val size: Int) {
  //SPL Specific part
  def transform(in: Array[Complex]): Array[Complex]
  def toLatex(): String = "No Latex defined"
  def toMatrix(): BlockFieldMatrix[Complex] = {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), size, size)
    for (i <- 0 until size) {
      val x = new Array[Complex](size)
      for (j <- 0 until size) {
        if (j == i)
          x.update(i, new Complex(1, 0.0))
        else
          x.update(j, new Complex(0.0, 0.0))
      }
      val outputrow = transform(x)
      m.setRow(i,outputrow)
    }
    m
  }
  def transformbyMatrix(in: Array[Complex]): Array[Complex] = {
    val x = new BlockFieldMatrix[Complex](ComplexField.getInstance(), in.size, 1)
    x.setColumn(1, in)
    val y = toMatrix().multiply(x)
    y.getColumn(1)
  }
  def pM() = MathUtilities.printm(toMatrix())
}

case class WHT(n: Int) extends SPL(n) {
  override def toLatex = "\\WHT(" + n + ")"
  override def toString = "WHT(" + n + ")";
  override def toMatrix() = {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), n, n)
    WHT_recurse(1, 1, 0, 0, n, n, m)
    def WHT_recurse(m: Int, sign: Int, x: Int, y: Int, x_top: Int, y_top: Int, matrix: BlockFieldMatrix[Complex]) {
      if (Math.pow(2, m) == n) {
        matrix.setEntry(x, y, new Complex(1 * sign, 0))
        matrix.setEntry(x + 1, y, new Complex(1 * sign, 0))
        matrix.setEntry(x, y + 1, new Complex(1 * sign, 0))
        matrix.setEntry(x + 1, y + 1, new Complex(-1 * sign, 0))
      }
      else {
        val cx = x_top - (x_top - x) / 2
        val cy = y_top - (y_top - y) / 2
        WHT_recurse(m + 1, sign, x, y, cx, cy, matrix)
        WHT_recurse(m + 1, sign, x, cy, cx, y_top, matrix)
        WHT_recurse(m + 1, sign, cx, y, x_top, cy, matrix)
        WHT_recurse(m + 1, -sign, cx, cy, x_top, y_top, matrix)
      }
    }
    m
  }

  def transform(in: Array[Complex]): Array[Complex] = transformbyMatrix(in)
}


/**
 * This is the basic building block ("the butterfly") of all DFT's - note the fixed size 2
 */
case class F_2() extends SPL(2) {
  override def toString = "F2"

  override def toLatex = "\\DFT_{2}"

  override def toMatrix() = {
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), 2, 2)
    m.setEntry(0, 0, new Complex(1, 0))
    m.setEntry(0, 1, new Complex(1, 0))
    m.setEntry(1, 0, new Complex(1, 0))
    m.setEntry(1, 1, new Complex(-1, 0))
    m
  }

  def transform(in: Array[Complex]): Array[Complex] = transformbyMatrix(in)
}


/**
 * Describes properties that all StridePermutations such as "L" share
 */
trait StridePermutation extends SPL {
  def transform(in: Array[Complex]): Array[Complex] = permute(in)
  def permute(in: Array[Complex]): Array[Complex]
  def transpose(order: List[Int]): List[Int] = {
    val t_array = new Array[Int](order.size)
    for (i <- 0 until order.size) {
      val pos = order(i)
      t_array(pos) = i
    }
    t_array.toList
  }
}

/**
 * Collects all common attributes of scaling matrices such as "I" and "T"
 */
trait DiagonalMatrix extends SPL {
  def transform(in: Array[Complex]): Array[Complex] = scale(in)
  def scale(in: Array[Complex]): Array[Complex]
}


/**
 * Identity matrix - usually only used in SPL for tensors and gone during SigmaSPL
 * @param n size
 */
case class I(n: Int) extends SPL(n) with StridePermutation with DiagonalMatrix {
  override def toString = "I(" + n + ")"
  override def toLatex = "\\one_{" + n + "}"
  override def transform(in: Array[Complex]): Array[Complex] = in
  override def scale(in: Array[Complex]): Array[Complex] = in
  override def permute(in: Array[Complex]): Array[Complex] = in
}

object MathUtilities {
  import scala.math._
  def kronecker (A: BlockFieldMatrix[Complex], B: BlockFieldMatrix[Complex] ): BlockFieldMatrix[Complex] = {
    val x = A.getRowDimension() * B.getRowDimension()
    val y = A.getColumnDimension() * B.getColumnDimension()
    val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), x, y)
    for (i <- 0 until x)
      for (j <- 0 until y)
      {
        val aentry = A.getEntry( (i/B.getRowDimension()), j/B.getColumnDimension() )
        val bentry = B.getEntry(i % B.getRowDimension(), j % B.getColumnDimension())
        m.setEntry(i,j, aentry.multiply(bentry))
      }
    m
  }

  def printm(m: org.apache.commons.math3.linear.BlockFieldMatrix[org.apache.commons.math3.complex.Complex]) = {
    for (i <- 0 until m.getRowDimension) {
      for (j <- 0 until m.getColumnDimension) {
        val x = m.getEntry(i, j)
        if (abs(x.getImaginary()) < 1E-14)
          m.setEntry(i, j, new org.apache.commons.math3.complex.Complex(x.getReal(), 0))
        val y = m.getEntry(i, j)
        if (abs(y.getReal()) < 1E-14)
          m.setEntry(i, j, new org.apache.commons.math3.complex.Complex(0, y.getImaginary))
        val t = m.getEntry(i, j)
        if (t.getReal() == 0 && t.getImaginary() == 0)
          print("(    ,    )")
        else
          print("(% 2.1f;% 2.1f)".format(t.getReal(), t.getImaginary))
      }
      println
    }
  }
  /*#############################################################################
##
#F DivisorPairs(x)
#F   returns a list of factorizations of x in two factors,
#F   excluding [1,x] and [x,1]. Return type is
#F   a list of lists (each with 2 integers).
#F*/
  def DivisorPairs(n: Int) =  {    (2 to Math.sqrt(n).toInt ).filter(n%_== 0).flatMap(x=>(if(n/x == x) List(x) else List(n/x,x)) ).toList.sortWith(_>_).map(x=>List(n/x,x))  }
}












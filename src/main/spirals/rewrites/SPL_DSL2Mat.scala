package ch.ethz.spirals.rewrites

import scala.lms._
import internal._

import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}

import org.apache.commons.math3.linear.BlockFieldMatrix
import org.apache.commons.math3.complex.{ComplexField, Complex}
import ch.ethz.spirals.dsls._

trait SPL_DSL2Mat extends Emit[Map[Int,BlockFieldMatrix[Complex]]] {
  self =>
  val IR: SPL_Exp with InternalFunctionsExp
  import IR._
  import MathUtilities._
   override def emitNode(tp: self.IR.TP[_], mmap: Map[Int, BlockFieldMatrix[Complex]],
                 block_callback: (self.IR.Block,Map[Int, BlockFieldMatrix[Complex]]) => Map[Int, BlockFieldMatrix[Complex]])
   : Map[Int, BlockFieldMatrix[Complex]] = {
      tp.rhs match{
        //--------------------------------Compose -----------------------------

        //-------------------------------Tensor--------------------------------

        //-------------------------------SPl Objects--------------------------------

        //------------------------------default traversal-----------------------------------------
        case InternalLambda(_,_,block,_,_) => block_callback(block,mmap)
        case _ => super.emitNode(tp,mmap,block_callback)
    }
  }
}



package ch.ethz.spirals.dsls

import ch.ethz.spirals.datatypes.DataTypeFactories.SplitComplexArray
import ch.ethz.spirals.datatypes.ElementOpsUnstaged.Complex
import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
import ch.ethz.spirals.datatypes._

import scala.lms._
import ops._
import scala.lms.internal.InternalFunctionsExp


case class MyComplex(re: Double, im: Double){
  def +(that: MyComplex) = MyComplex(this.re + that.re, this.im + that.im)
  def -(that: MyComplex) = MyComplex(this.re - that.re, this.im - that.im)
}

trait StagedScala_Exp extends PureNumericOpsExp with BaseExp with InternalFunctionsExp with TypeClassesStagedNumericOps with TypeClassesStagedArrayOps{
  case class StagedComplex(re: Exp[Double], im: Exp[Double]){
    def +(that: StagedComplex) = StagedComplex(this.re + that.re, this.im + that.im)
    def -(that: StagedComplex) = StagedComplex(this.re - that.re, this.im - that.im)
  }
}


class StagedScala_DSL extends StagedScala_Exp

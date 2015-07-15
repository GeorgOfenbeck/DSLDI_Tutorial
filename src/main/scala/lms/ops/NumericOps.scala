package scala.lms
package ops

import java.io.PrintWriter
import org.scala_lang.virtualized.SourceContext
import scala.lms.internal.Expressions
import util.OverloadHack

trait LiftNumeric {
 this: Base =>
 implicit def numericToNumericRep[T:Numeric:Manifest](x: T) = unit(x)
}

trait PureNumericOps extends ImplicitOps {
 implicit def numericToNumericOps[T:Numeric:Manifest](n: T) = new NumericOpsCls(unit(n))
 implicit def repNumericToNumericOps[T:Numeric:Manifest](n: Rep[T]) = new NumericOpsCls(n)

 class NumericOpsCls[T:Numeric:Manifest](lhs: Rep[T]){
  def +[A](rhs: A)(implicit c: A => T, pos: SourceContext) = numeric_plus(lhs,unit(c(rhs)))
  def +(rhs: Rep[T])(implicit pos: SourceContext) = numeric_plus(lhs,rhs)
  def -(rhs: Rep[T])(implicit pos: SourceContext) = numeric_minus(lhs,rhs)
  def *(rhs: Rep[T])(implicit pos: SourceContext) = numeric_times(lhs,rhs)
  def /(rhs: Rep[T])(implicit pos: SourceContext) = numeric_divide(lhs,rhs)
 }

 def numeric_plus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
 def numeric_minus[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
 def numeric_times[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
 def numeric_divide[T:Numeric:Manifest](lhs: Rep[T], rhs: Rep[T])(implicit pos: SourceContext): Rep[T]
 //def numeric_negate[T:Numeric](x: T): Rep[T]
 //def numeric_abs[T:Numeric](x: T): Rep[T]
 //def numeric_signum[T:Numeric](x: T): Rep[Int]
}

trait PureNumericOpsExp extends PureNumericOps with ImplicitOpsExp with Expressions{
 abstract class DefMN[A:Manifest:Numeric] extends Def[A] {
  def mev = manifest[A]
  def aev = implicitly[Numeric[A]]
 }

 case class NumericPlus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
 case class NumericMinus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
 case class NumericTimes[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]
 case class NumericDivide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T]) extends DefMN[T]

 def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericPlus(lhs, rhs)
 def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericMinus(lhs, rhs)
 def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericTimes(lhs, rhs)
 def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext) : Exp[T] = NumericDivide(lhs, rhs)
}


trait PureNumericOpsExpOpt extends PureNumericOpsExp {
 override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
  case (Const(x: T), Const(y: T)) => Const(implicitly[Numeric[T]].plus(x,y))
  case (Const(x), y) if x == implicitly[Numeric[T]].zero => y
  case (x, Const(y)) if y == implicitly[Numeric[T]].zero => x
  case _ => super.numeric_plus(lhs,rhs)
 }
 override def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
  case (Const(x: T), Const(y: T)) => Const(implicitly[Numeric[T]].minus(x,y))
  case _ => super.numeric_minus(lhs,rhs)
 }
 override def numeric_times[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
  case (Const(x: T), Const(y: T)) => Const(implicitly[Numeric[T]].times(x,y))
  case (Const(x: T), y) if x == implicitly[Numeric[T]].zero => Const(x)
  case (x, Const(y: T)) if y == implicitly[Numeric[T]].zero => Const(y)
  case (Const(x), y) if x == implicitly[Numeric[T]].one => y
  case (x, Const(y)) if y == implicitly[Numeric[T]].one => x
  case _ => super.numeric_times(lhs,rhs)
 }
 override def numeric_divide[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = (lhs,rhs) match {
  // CAVEAT: Numeric doesn't have .div, Fractional has
  case (Const(x: T), Const(y: T)) => Const(implicitly[Numeric[T]].asInstanceOf[Fractional[T]].div(x,y))
  case _ => super.numeric_divide(lhs,rhs)
 }
}


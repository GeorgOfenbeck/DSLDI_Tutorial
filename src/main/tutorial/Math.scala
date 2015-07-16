package tutorial

import scala.lms.internal.Expressions


//thats the typeclass
abstract class NumericOps[T]/*(implicit val tag: Manifest[T])*/ {
  def plus(x: T, y: T): T

  def fromInt(x: Int): T

}

object myimplicits{
  implicit object UnstagedEvidence extends NumericOps[Double] {
    def plus(x: Double, y: Double): Double = x + y
    def fromInt(x: Int) = x.toDouble
  }
}


//once you import LMS you have Exp[T], Def[T] and TP(exp: Exp[T], def: Def[T])
trait StagedNumeric extends Expressions{
  //Expressions defines the toAtom Method that converts Defs
  //to TP's by giving it a new Exp(symbolnumber) and returning a TP
  case class NumericPlus[T:NumericOps](lhs: Exp[T], rhs: Exp[T]) extends Def[T]
  def numeric_plus[T:NumericOps:Manifest](lhs: Exp[T], rhs: Exp[T]): Exp[T] = toAtom(NumericPlus(lhs, rhs))

  case class MyConst[T](x: T) extends Def[T]
  def myconst[T: Manifest](x: T): Exp[T] = toAtom(MyConst(x))

  import myimplicits._
  implicit object StagedEvidence extends NumericOps[Exp[Double]]{
    def plus (x: Exp[Double], y: Exp[Double]): Exp[Double] = numeric_plus(x,y)
    def fromInt(x: Int): Exp[Double] = myconst(x)
  }


}




class MyDSL extends StagedNumeric {

  def mathfunction[T](x: T)(implicit ntypeclass: NumericOps[T]): T = {
    ntypeclass.plus(x, x)
  }

}

object Math extends App{

  val dslobject = new MyDSL
  val dslobject2 = new MyDSL




  val input: dslobject.Exp[Double] = dslobject.myconst(3.0)
  import dslobject._
  println(dslobject.mathfunction(input))
  import myimplicits._
  println(dslobject.mathfunction(3.0))
}

package tutorial

import scala.lms.internal.Expressions


//thats the typeclass
abstract class NumericOps[T,S[T]]/*(implicit val tag: Manifest[T])*/ {
  def plus(x: S[T], y: S[T]): S[T]
  def fromInt(x: Int): S[T]
}
abstract class VectorOps[S[_], V[_], T] {
  def apply(x: S[V[T]], i: S[Int]): S[T]
  def ini(from: Seq[S[T]]): S[V[T]]
  def unit(x: Int): S[Int]
}



object myimplicits{
  type id[T] = T
  implicit object UnstagedEvidence extends NumericOps[Double,id] {
    def plus(x: Double, y: Double): Double = x + y
    def fromInt(x: Int) = x.toDouble
  }

  class UnstagedVectorEvidence[T] extends VectorOps[id,Vector,T]{
    def apply(x: id[Vector[T]], i: id[Int]): id[T] = x(i)
    def ini(from: Seq[id[T]]): id[Vector[T]] = from.toVector
    def unit(x: Int): id[Int] = x
  }

  implicit object UnstagedDoubleVectorEvidence extends UnstagedVectorEvidence[Double]

}


//once you import LMS you have Exp[T], Def[T] and TP(exp: Exp[T], def: Def[T])
trait StagedNumeric extends Expressions{
  //Expressions defines the toAtom Method that converts Defs
  //to TP's by giving it a new Exp(symbolnumber) and returning a TP
  case class NumericPlus[T](lhs: Exp[T], rhs: Exp[T])(implicit ntype: NumericOps[T,Exp]) extends Def[T]
  def numeric_plus[T](lhs: Exp[T], rhs: Exp[T])(implicit ntype: NumericOps[T,Exp], tag :Manifest[T]): Exp[T] = toAtom(NumericPlus(lhs, rhs))

  case class VectorFromSeq[T:Manifest](xs: Seq[Exp[T]]) extends Def[Vector[T]]
  case class VectorApply[T:Manifest](a: Exp[Vector[T]], n: Exp[Int]) extends Def[T]
  def vector_obj_fromseq[T:Manifest](xs: Seq[Exp[T]]): Exp[Vector[T]] = VectorFromSeq(xs)
  def vector_apply[T:Manifest](x: Exp[Vector[T]], n: Exp[Int]): Exp[T] = VectorApply(x, n)

  case class MyConst[T](x: T) extends Def[T]
  def myconst[T: Manifest](x: T): Exp[T] = toAtom(MyConst(x))

  import myimplicits._
  implicit object StagedEvidence extends NumericOps[Double,Exp]{
    def plus (x: Exp[Double], y: Exp[Double]): Exp[Double] = numeric_plus(x,y)
    def fromInt(x: Int): Exp[Double] = myconst(x)
  }

  class StagedVectorEvidence[T: Manifest] extends VectorOps[Exp,Vector,T]{
    def apply(x: Exp[Vector[T]], i: Exp[Int]): Exp[T] = vector_apply(x,i)
    def ini(from: Seq[Exp[T]]): Exp[Vector[T]] = vector_obj_fromseq(from)
    def unit(x: Int): Exp[Int] = myconst(x)
  }

  implicit object StagedDoubleVectorEvidence extends StagedVectorEvidence[Double]



}




class MyDSL extends StagedNumeric {

  def mathfunction[S[_],T](x: S[Vector[T]])(implicit ntypeclass: NumericOps[T,S], vtypeclass: VectorOps[S,Vector,T]): S[Vector[T]] = {
    import ntypeclass._
    import vtypeclass._
    val idx1:S[Int] = vtypeclass.unit(0)
    val idx2:S[Int] = vtypeclass.unit(1)
    val ele1 = apply(x,idx1)
    val ele2 = apply(x,idx2)
    val t1 = plus(ele1,ele2)
    val t2 = plus(ele1,ele2)
    ini(Seq(t1,t2))
  }

}

object Math extends App{

  val dslobject = new MyDSL
  val staged_seq: Seq[dslobject.Exp[Double]] = Seq(dslobject.myconst(3.0),dslobject.myconst(4.0))
  val staged_input: dslobject.Exp[Vector[Double]] =
    dslobject.vector_obj_fromseq(staged_seq)



  //import myimplicits._

  val ev1:  NumericOps[Double,myimplicits.id] = myimplicits.UnstagedEvidence
  val uvector: myimplicits.id[Vector[Double]] = Vector(1.1,2.2)
  dslobject.mathfunction(uvector)(ev1,myimplicits.UnstagedDoubleVectorEvidence)
  import dslobject._
  dslobject.mathfunction(staged_input)//(dslobject.StagedEvidence,dslobject.Stag)
  println("...")
}

package scala.lms
package internal

import scala.lms.BaseExp



trait ReificationPure{
 val IR: BaseExp with InternalFunctionsExp
 val sym2tp: Map[IR.Exp[_], IR.TP[_]]
 val def2tp: Map[IR.Def[_], IR.TP[_]]
 val id2tp: Map[Int, IR.TP[_]]
 val rootlambda: IR.InternalLambda[_,_]
 val fun2tp: Map[(_ => _), IR.TP[_]]

 object Const {
  def unapply[T](e: IR.Exp[T]): Option[IR.ConstDef[T]] = {
   val tp = sym2tp(e)
   tp.rhs match {
    case d@IR.ConstDef(x) => Some(d.asInstanceOf[IR.ConstDef[T]]) //TODO - get rid of type cast
    case _ => None
   }
  }
 }

 object Arg {
  def unapply[T](e: IR.Exp[T]): Option[IR.ArgDef[T]] = {
   val tp = sym2tp(e)
   tp.rhs match {
    case d@IR.ArgDef(id) => Some(d.asInstanceOf[IR.ArgDef[T]]) //TODO - get rid of type cast
    case _ => None
   }
  }
 }
}


trait ReifyPure{
 self =>
 val IR: BaseExp with InternalFunctionsExp
 import IR._

 def reifyProgram[A,R](f: Function1[A,R])(implicit args: ExposeRep[A], returns: ExposeRep[R]): ReificationPure{ val IR: self.IR.type} = {
  IR.reset()
  val lambda = IR.fun(f)
  val lambdatp: IR.TP[_] = IR.fun2tp(lambda)
  reifyProgramfromLambda(lambdatp)
 }

 def reifyProgramfromLambda(lambdatp: TP[_]): ReificationPure{ val IR: self.IR.type} = {
  //val tp = exp2tp(lambda)
  val lam: InternalLambda[_,_] = lambdatp.rhs match{
   case x@InternalLambda(_,_,block,_,_) => x
   case _ => {
    assert(false, "This should not be possible")
    ???
   }
  }

  val immutable_out = new ReificationPure {
   val IR: self.IR.type = self.IR
   val sym2tp: Map[IR.Exp[_], IR.TP[_]] = self.IR.exp2tp
   val def2tp: Map[IR.Def[_], IR.TP[_]] = self.IR.def2tp
   val id2tp: Map[Int,IR.TP[_]] = self.IR.id2tp
   val fun2tp: Map[(_ => _), IR.TP[_]] = self.IR.fun2tp


   //val rootblock = lam.y
   val rootlambda = lam
  }
  immutable_out
 }
}
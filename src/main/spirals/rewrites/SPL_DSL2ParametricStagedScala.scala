package ch.ethz.spirals.rewrites
import ch.ethz.spirals.dsls._
import scala.lms._
import internal._
import ch.ethz.spirals.datatypes._


trait SPL_DSL2ParametricStagedScala[V[_],E[_],R[_],T] extends Emit[Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]]] {
  self =>
  val IR: SPL_Exp with InternalFunctionsExp
  val targetIR: StagedScala_Exp
  override def emitNode(tp: self.IR.TP[_], fmap: Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]],
                        block_callback: (self.IR.Block, Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]]) => Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]])
  : Map[Int, CVector[V,E,R,T] => CVector[V,E,R,T]] = {
    tp.rhs match {
      //--------------------------------Compose -----------------------------
      case IR.Compose(IR.Exp(a), IR.Exp(b), size) => {
        val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (in: CVector[V,E,R,T]) => fmap(a)(fmap(b)(in))
        fmap + (tp.sym.id -> f)
      }
      //-------------------------------Tensor--------------------------------
      case IR.Tensor(IR.Exp(a), IR.Exp(b), size) => {
        val f: CVector[V,E,R,T] => CVector[V,E,R,T] = (IR.id2tp(a).rhs, IR.id2tp(b).rhs) match {
          case (IR.ConstDef(I(n)), _) => {
            (in: CVector[V,E,R,T]) => {
              val base = 0
              val strides = Vector(1,n)
              val h: (Vector[Int]) => Int = (loopvars: Vector[Int]) => { //this is the stride function we access with
                base + loopvars.zip(strides).foldLeft(0)({(acc,ele) => acc + (ele._1 * ele._2)})}
              val A = fmap(b)
              in.GT(A,h,h,Vector(size/n,n))(in) //in.grouped(size / n).flatMap(chunk => fmap(b)(chunk)).toVector
            }
          }
          case (_, IR.ConstDef(I(n))) => {
            (in: CVector[V,E,R,T]) => {
              val base = 0
              val strides = Vector(size/n,1)
              val h: (Vector[Int]) => Int = (loopvars: Vector[Int]) => { //this is the stride function we access with
                  base + loopvars.zip(strides).foldLeft(0)({(acc,ele) => acc + (ele._1 * ele._2)})}
              val A = fmap(b)
              in.GT(A,h,h,Vector(size/n,n))(in) //in.grouped(n).toList.transpose.map(chunk => fmap(a)(chunk.toVector)).transpose.flatten.toVector
            }
          }
          case _ => ??? //we dont support anything else for this tutorial
        }
        fmap + (tp.sym.id -> f)
      }
      //-------------------------------SPl Objects--------------------------------
      case IR.ConstDef(x: SPL) => {
        val f: CVector[V,E,R,T] => CVector[V,E,R,T] =
          x match {
            case I(n) => (in: CVector[V,E,R,T]) => in
            case F_2() => (in: CVector[V,E,R,T]) => {
                val t0 = in(in.irep.fromInt(0))
                val t1 = in(in.irep.fromInt(1))
                val res1 = in.erep.plus(t0,t1)
                val res2 = in.erep.minus(t0,t1)
                in.ini(Vector(res1, res2))

              }
            case _ => ??? //we dont support anything else for this tutorial
          }
        fmap + (tp.sym.id -> f)
      }
      //------------------------------default traversal-----------------------------------------
      case IR.InternalLambda(_, _, block, _, _) => block_callback(block, fmap)
      case _ => super.emitNode(tp, fmap, block_callback)
    }
  }
}

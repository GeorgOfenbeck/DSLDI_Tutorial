package ch.ethz.spirals.rewrites
import ch.ethz.spirals.dsls._
import scala.lms._
import internal._


  trait SPL_DSL2StagedScalaW extends  {
    val targetIR: StagedScala_Exp
    val originIR: SPL_Exp with InternalFunctionsExp
    trait SPL_DSL2StagedScala extends Emit[Map[Int, Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex]]] {
      self =>
      val IR: originIR.type = originIR
      override def emitNode(tp: self.IR.TP[_], fmap: Map[Int, Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex]],
                            block_callback: (self.IR.Block, Map[Int, Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex]]) => Map[Int, Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex]])
      : Map[Int, Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex]] = {
        tp.rhs match {
          //--------------------------------Compose -----------------------------
          case IR.Compose(IR.Exp(a), IR.Exp(b), size) => {
            val f: Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex] = (in: Vector[targetIR.StagedComplex]) => fmap(a)(fmap(b)(in))
            fmap + (tp.sym.id -> f)
          }
          //-------------------------------Tensor--------------------------------
          case IR.Tensor(IR.Exp(a), IR.Exp(b), size) => {
            val f: Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex] = (IR.id2tp(a).rhs, IR.id2tp(b).rhs) match {
              case (IR.ConstDef(I(n)), _) => {
                (in: Vector[targetIR.StagedComplex]) =>
                  in.grouped(size / n).flatMap(chunk => fmap(b)(chunk)).toVector
              }
              case (_, IR.ConstDef(I(n))) => {
                (in: Vector[targetIR.StagedComplex]) =>
                  in.grouped(n).toList.transpose.map(chunk => fmap(a)(chunk.toVector)).transpose.flatten.toVector
              }
              case _ => ??? //we dont support anyting else for this tutorial
            }
            fmap + (tp.sym.id -> f)
          }
          //-------------------------------SPl Objects--------------------------------
          case IR.ConstDef(x: SPL) => {
            val f: Vector[targetIR.StagedComplex] => Vector[targetIR.StagedComplex] =
              x match {
                case I(n) => (in: Vector[targetIR.StagedComplex]) => in
                case F_2() => (in: Vector[targetIR.StagedComplex]) => {
                  import targetIR._
                  Vector(in(0) + in(1), in(0) - in(1))
                }
                case _ => ??? //we dont support anyting else for this tutorial
              }
            fmap + (tp.sym.id -> f)
          }
          //------------------------------default traversal-----------------------------------------
          case IR.InternalLambda(_, _, block, _, _) => block_callback(block, fmap)
          case _ => super.emitNode(tp, fmap, block_callback)
        }
      }
    }
    def translater() = {
      new SPL_DSL2StagedScala {
        override val IR: originIR.type = originIR
      }
    }
  }


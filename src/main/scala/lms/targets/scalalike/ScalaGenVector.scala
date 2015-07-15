package scala.lms
package targets
package scalalike

import ops.VectorOpsExp

trait ScalaGenVectorOps extends ScalaCodegen{
  val IR: VectorOpsExp with internal.InternalFunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block,String) => String): String = {
    val ma = tp.rhs match {
      case VectorFromSeq(xs) => {
        val args = xs.map(x => quote(x)).mkString(",")
        emitValDef(tp, src"Vector($args)")
      }
      case VectorApply(x,n) => emitValDef(tp, src"$x($n)")
      case _ => super.emitNode(tp,acc,block_callback)
    }
    ma
  }
}

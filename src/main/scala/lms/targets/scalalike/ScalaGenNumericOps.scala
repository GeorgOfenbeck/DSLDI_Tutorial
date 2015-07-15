

package scala.lms
package targets
package scalalike

import ops._

trait ScalaGenNumericOps extends ScalaCodegen{
  val IR: PureNumericOpsExp with internal.InternalFunctionsExp
  import IR._

  override def emitNode(tp: TP[_], acc: String,
                        block_callback: (Block,String) => String): String = {
    val ma = tp.rhs match {
      case NumericPlus(lhs,rhs) => emitValDef(tp,src"$lhs + $rhs")
      case NumericMinus(lhs,rhs) => emitValDef(tp,src"$lhs - $rhs")
      case _ => super.emitNode(tp,acc,block_callback)
    }
    ma
  }
}


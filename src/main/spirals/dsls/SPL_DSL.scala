package ch.ethz.spirals.dsls
import scala.lms._
import scala.lms.internal.InternalFunctionsExp

trait SPL_DSL extends BaseExp with InternalFunctionsExp {

  implicit def SPLtoExp(i: SPL): Exp[SPL] = unit(i)

  def tensor(x: Exp[SPL], y: Exp[SPL]): Exp[SPL] = {
    val tmp = Tensor(x, y)
    tmp
  }
  def compose(x: Exp[SPL], y: Exp[SPL]): Exp[SPL] = Compose(x, y)

  case class Tensor(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]
  case class Compose(x: Exp[SPL], y: Exp[SPL]) extends Def[SPL]
}


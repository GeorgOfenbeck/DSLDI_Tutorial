package ch.ethz.spirals.rewrites

trait BreakDown2SPL_DSL{
  import ch.ethz.spirals.dsls._

  val IR: SPL_Exp
  import IR._
  def bd2spl(in_bd: BreakDown): Rep[SPL] = {
    val r: Rep[SPL] = in_bd.applied_bd map (x =>
      x.rule match {
        case BreakdownRules.WHT_CT => {
          val k1 = x.children(0).nt.size //divisors(0)
          val d1 = x.children(1).nt.size //divisors(1)
          val n1 = k1 * d1
          val extended_children = x.children map (child => bd2spl(child))
          val c1: Rep[SPL] = extended_children(0)
          val c2: Rep[SPL] = extended_children(1)
          val spl_expression = {
            //SPL - this is the actual SPL expression - c1 and c2 are the possibly extended children
            import IR.SPLOps
            ( c1 tensor unit(I(d1)) ) compose ( unit(I(k1)) tensor c2)
            //--------------------------------------------------------------------------------
          }
          spl_expression
        }
        case BreakdownRules.WHT_Base => {
          val spl: Rep[SPL] = unit(F_2())
          spl
        }
        case _ =>
          println(x)
          assert(false);
          val spl: Rep[SPL] = unit(F_2())
          spl
      }
      ) getOrElse (
      {
        //assert(false);
        val spl: Rep[SPL] = unit(F_2())
        spl
      }
      )
    r
  }
}
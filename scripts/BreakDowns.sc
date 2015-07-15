import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._



val wht4 = WHT(4)

val wht4bd = BreakdownRules.genRandomBreakDown(wht4)
println(wht4bd.sample)

val wht512bd = BreakdownRules.genRandomBreakDown(WHT(512))
println(wht512bd.sample)
println(wht512bd.sample)

val wht512tree = BreakdownRules.genRandomRuleTree(WHT(512))

println(wht512tree.sample)
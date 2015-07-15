import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._



val wht4 = WHT(4)

val wht4bd = BreakdownRules.genRandomBreakDown(wht4)

println(wht4bd.sample)
val wht128 = WHT(128)
val wht128bd = BreakdownRules.genRandomBreakDown(wht128)
println(wht128bd.sample)
val wht128tree = BreakdownRules.genRandomRuleTree(wht128)
println(wht128tree.sample)


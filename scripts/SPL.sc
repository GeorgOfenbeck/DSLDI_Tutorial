

import ch.ethz.spirals.dsls._
import org.apache.commons.math3.complex.{ComplexField, Complex}
import org.apache.commons.math3.linear.BlockFieldMatrix

val wht4 = WHT(4) //Walsh–Hadamard transform size 4
wht4.toString() //
wht4.toMatrix().toString
MathUtilities.printm(wht4.toMatrix())
val f2 = F_2()
val i2 = I(2)
val wht_composed_1 = MathUtilities.kronecker(f2.toMatrix(),i2.toMatrix())
MathUtilities.printm(wht_composed_1)
val wht_composed_2 = MathUtilities.kronecker(i2.toMatrix(),f2.toMatrix())
MathUtilities.printm(wht_composed_2)
val wht_composed_complete = wht_composed_1.multiply(wht_composed_2)
MathUtilities.printm(wht_composed_complete)


val size = 4
val inv = new Array[org.apache.commons.math3.complex.Complex](size)
for (i <- 0 until 4) {
  inv.update(i,new org.apache.commons.math3.complex.Complex(1, 1))
}
val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), size, 1)
m.setColumn(0,inv)
MathUtilities.printm(m)
val resbydef = wht_composed_complete.multiply(m)
MathUtilities.printm(resbydef)
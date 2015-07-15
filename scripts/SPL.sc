
import ch.ethz.spirals.dsls._
import org.apache.commons.math3.complex.{ComplexField, Complex}
import org.apache.commons.math3.linear.BlockFieldMatrix

import MathUtilities._
val wht4 = WHT(4)
val mwht = wht4.toMatrix()
printm(mwht)


val f2 = F_2()
val i2 = I(2)

val leftside = MathUtilities.kronecker(f2.toMatrix(), i2.toMatrix())
val rightside = MathUtilities.kronecker(i2.toMatrix(),f2.toMatrix())

printm(leftside)
printm(rightside)

val wht_composed = leftside.multiply(rightside)
printm(wht_composed)


val size = 4
val inv = new Array[org.apache.commons.math3.complex.Complex](size)
for (i <- 0 until 4) {
  if (i == 2)
    inv.update(i,new org.apache.commons.math3.complex.Complex(1, 0))
  else
    inv.update(i,new org.apache.commons.math3.complex.Complex(0, 0))
}
val m = new BlockFieldMatrix[Complex](ComplexField.getInstance(), size, 1)
m.setColumn(0,inv)

val res = wht_composed.multiply(m)
printm(res)


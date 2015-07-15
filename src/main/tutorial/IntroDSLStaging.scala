package tutorial

import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._

object IntroDSLStaging extends App {

//  val wht4 = (F_2 tensor I(2) ) compose (I(2) tensor F_2) //what we would like to have
  class MyDSLProgram extends SPL_DSL{

    def myprog() = {
      val leftside = tensor(F_2(), I(2))
      val rightside = tensor(I(2), F_2())
      val wht4 = compose(leftside, rightside)
      wht4
    }
  }

  val dslprogram = new MyDSLProgram
  val result = dslprogram.myprog()
  println(result)


}

package tutorial

import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._

import scala.lms.targets.graphviz.GraphVizExport

object IntroDSLStaging extends App {

//  val wht4 = (F_2 tensor I(2) ) compose (I(2) tensor F_2) //what we would like to have
  class MyDSLProgram extends SPL_DSL{
    self =>
    def myprog(u: Rep[Unit]) = {
      val leftside = tensor(F_2(), I(2))
      val rightside = tensor(I(2), F_2())
      val wht4 = compose(leftside, rightside)
      wht4
    }

    def graphvizexport() = {
      val (code, cm) = emitGraph.emitDepGraphf(myprog)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("spl_SON.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    val emitGraph = new GraphVizExport {
    override val IR: self.type = self
  }


}

  val dslprogram = new MyDSLProgram
  //val result = dslprogram.myprog()
  //println(result)


  dslprogram.graphvizexport()



}

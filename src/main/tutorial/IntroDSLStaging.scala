package tutorial

import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._

import scala.lms.internal.InternalFunctionsExp
import scala.lms.targets.graphviz.GraphVizExport

object IntroDSLStaging extends App {

  //  val wht4 = (F_2 tensor I(2) ) compose (I(2) tensor F_2) //what we would like to have
  class MyDSLProgram extends SPL_DSL {
    self =>

    def myprog(u: Rep[Unit]): Rep[SPL] = {
      val i2: Rep[SPL] = I(2)
      val f2: Rep[SPL] = F_2()
      (f2 tensor i2) compose (i2 tensor f2)
    }

    def graphvizexport() = {
      val (code, cm) = emitGraph.emitDepGraphf(myprog)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("spl_SON.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    def printMat() = {
        val (map,cm) = emitMat.emit(Map.empty,myprog)
        for ((key,matrix) <- map) {
          println("matrix: " + key)
          MathUtilities.printm(matrix)

      }
    }

    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }

    val emitMat = new SPL_DSL2Mat {
      override val IR: self.type = self
    }


  }

  val dslprogram = new MyDSLProgram
  //val result = dslprogram.myprog()
  //println(result)


  dslprogram.graphvizexport()
  dslprogram.printMat()


}

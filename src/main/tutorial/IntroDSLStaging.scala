package tutorial

import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._
import ch.ethz.spirals.datatypes._


import scala.lms.internal._
import scala.lms.ops._
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._

object IntroDSLStaging extends App {

  //  val wht4 = (F_2 tensor I(2) ) compose (I(2) tensor F_2) //what we would like to have
  class MyDSLProgram extends SPL_DSL {
    self =>

    val newIR = new PureNumericOpsExpOpt with InternalFunctionsExp with ScalaCompile with DontLookinHere{
      self =>
      val codegen = new ScalaCodegen with EmitHeadInternalFunctionAsClass with ScalaGenNumericOps with ScalaGenVectorOps {
        val IR: self.type = self
      }
    }
    val emitStagedScala = new SPL_DSL2StagedScalaW {
      override val originIR: self.type = self
      override val targetIR: newIR.type = newIR
    }


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

    def getScalaFunction () = {
      val (map, cm) = emitScala.emit(Map.empty,myprog)
      val toplevelf = map(map.keysIterator.max)
      val inputComplexVector = Vector(MyComplex(1.0, 1.0), MyComplex(1.0, 1.0), MyComplex(1.0, 1.0), MyComplex(1.0, 1.0))
      val output = toplevelf(inputComplexVector)
      println(output)
    }

    def stageTheScalaFunction() = {
      val iemit = new emitStagedScala.SPL_DSL2StagedScala {} //thats pretty ugly
      val (map, cm) = iemit.emit(Map.empty, myprog)
      val toplevelf = map(map.keysIterator.max)
      val f = (in: newIR.ComplexVector) => newIR.ComplexVector(toplevelf(in.vec))

      val emitGraphScala = new GraphVizExport {
        override val IR: newIR.type = newIR
      }
      val (scalagraph, cm2) = {
        import newIR._
        emitGraphScala.emitDepGraphf(f)(exposeRepFromVComplex(4), exposeRepFromVComplex(4))
      }
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_SON.dot"))
      stream.println(scalagraph)
      stream.flush()
      stream.close()
    }


    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }

    val emitMat = new SPL_DSL2Mat {
      override val IR: self.type = self
    }

    val emitScala = new SPL_DSL2Scala {
      override val IR: self.type = self
    }




  }

  val dslprogram = new MyDSLProgram
  //val result = dslprogram.myprog()
  //println(result)


  dslprogram.graphvizexport()
  dslprogram.printMat()
  dslprogram.getScalaFunction()
  dslprogram.stageTheScalaFunction()
}

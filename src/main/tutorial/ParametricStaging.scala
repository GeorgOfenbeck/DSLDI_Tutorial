package tutorial
import ch.ethz.spirals.datatypes.DataTypeFactories.{InterleavedComplexArray, SplitComplexArray}
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.rewrites._
import scala.lms.internal.InternalFunctionsExp
import scala.lms.ops.PureNumericOpsExpOpt
import scala.lms.targets.graphviz.GraphVizExport
import scala.lms.targets.scalalike._
import ch.ethz.spirals.datatypes._
import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
import ElementOpsUnstaged._
object ParametricStaging extends App {

  class MyDSLProgram extends SPL_DSL{
    self =>

    val whtsize = 128

    def myf(u: Rep[Unit]): Rep[SPL] = {
      val bd = BreakdownRules.genRandomRuleTree(WHT(whtsize))
      break2spl.bd2spl(bd.sample.get)
    }

    val break2spl = new BreakDown2SPL_DSL {
      override val IR: self.type = self
    }

    val emitGraph = new GraphVizExport {
      override val IR: self.type = self
    }

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

    def exportgraph() = {
      val (code, cm) = emitGraph.emitDepGraphf(myf)
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("spl_SON.dot"))
      stream.println(code)
      stream.flush()
      stream.close()
    }

    def doNextLevel() = {
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = whtsize
      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[NoRep, ElementOpsUnstaged.Complex,newIR.Rep,Double]{
          override val IR: self.type = self
          override val targetIR: newIR.type = newIR
        }
      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      val expose = newIR.exposeRepFromScalarSplitComplex(whtsize)
      val emitGraphScala = new GraphVizExport {
        override val IR: newIR.type = newIR
      }
      val (scalagraph, cm2) = {
        emitGraphScala.emitDepGraphf(toplevelf)(expose,expose)
      }
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_scalar_SON.dot"))
      stream.println(scalagraph)
      stream.flush()
      stream.close()

      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("source.txt"))
      val esc = newIR.codegen.emitSource(toplevelf,"testClass",stream2)(expose,expose)
      stream2.flush()
      stream2.close()
    }
  }


  val myprog = new MyDSLProgram
  myprog.exportgraph()
  myprog.doNextLevel()

}
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

object Main extends App {

  class MyDSLProgram extends SPL_DSL {
    self =>


    def myf(u: Rep[Unit]) = {
      val f2: Exp[SPL] = unit(F_2())
      val i2: Exp[SPL] = unit(I(2))
      (f2 tensor i2) compose (i2 tensor f2)
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

    def createMat() = {
      val (map, cm) = emitMat.emit(Map.empty, myf)
      for ((key, matrix) <- map) {
        println("matrix: " + key)
        MathUtilities.printm(matrix)
      }
    }

    def createScalaCode() = {
      val (map, cm) = emitScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      val inputComplexVector = Vector(MyComplex(1.0, 1.0), MyComplex(1.0, 1.0), MyComplex(1.0, 1.0), MyComplex(1.0, 1.0))
      val output = toplevelf(inputComplexVector)
      println(output)
    }

    def createStagedScalaCode() = {
      val iemit = new emitStagedScala.SPL_DSL2StagedScala {} //thats pretty ugly
      val (map, cm) = iemit.emit(Map.empty, myf)
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

    def createParamizedNonStagedScalaCode() = {
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4
      val input = new SplitComplexArray[NoRep, Array, NoRep, Double](size)
      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[NoRep, ElementOpsUnstaged.Complex,NoRep,Double]{
        override val IR: self.type = self
        override val targetIR: newIR.type = newIR
      }
      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      val out = toplevelf(input)
      println(out)
      println("---")
    }


    def createParamizedStagedScalarScalaCode() = {
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4
      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[NoRep, ElementOpsUnstaged.Complex,newIR.Rep,Double]{
          override val IR: self.type = self
          override val targetIR: newIR.type = newIR
        }
      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      val expose = newIR.exposeRepFromScalarSplitComplex(4)
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


    def createParamizedStagedInterleavedScalaCode() = {
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4

      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[newIR.Rep, ElementOpsUnstaged.Complex,newIR.NoRep,Double]{
          override val IR: self.type = self
          override val targetIR: newIR.type = newIR
        }

      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)

      val expose = newIR.exposeRepFromStagedInterleaved(4)

      val emitGraphScala = new GraphVizExport {
        override val IR: newIR.type = newIR
      }
      val (scalagraph, cm2) = {
        emitGraphScala.emitDepGraphf(toplevelf)(expose,expose)
      }
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_intercomplex_SON.dot"))
      stream.println(scalagraph)
      stream.flush()
      stream.close()

      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("source_intercomplex.txt"))
      val esc = newIR.codegen.emitSource(toplevelf,"testClass",stream2)(expose,expose)
      stream2.flush()
      stream2.close()

      println("---")
    }

    def createParamizedStagedSplitComplexCode() = {
      import ch.ethz.spirals.datatypes._
      import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
      import ElementOpsUnstaged._
      val size: ch.ethz.spirals.datatypes.UnstagedImplicitOps.NoRep[Int] = 4



      val emitParametricStagedScala =
        new SPL_DSL2ParametricStagedScala[newIR.Rep, ElementOpsUnstaged.Complex,newIR.NoRep,Double]{
          override val IR: self.type = self
          override val targetIR: newIR.type = newIR
        }

      val (map, cm) = emitParametricStagedScala.emit(Map.empty, myf)
      val toplevelf = map(map.keysIterator.max)
      //toplevelf(input)



      val expose = newIR.exposeRepFromSplitComplex(4)

      val emitGraphScala = new GraphVizExport {
        override val IR: newIR.type = newIR
      }
      val (scalagraph, cm2) = {
        emitGraphScala.emitDepGraphf(toplevelf)(expose,expose)
      }
      val stream = new java.io.PrintWriter(new java.io.FileOutputStream("scala_splitcomplex_SON.dot"))
      stream.println(scalagraph)
      stream.flush()
      stream.close()

      val stream2 = new java.io.PrintWriter(new java.io.FileOutputStream("source_splitcomplex.txt"))
      val esc = newIR.codegen.emitSource(toplevelf,"testClass",stream2)(expose,expose)
      stream2.flush()
      stream2.close()

      println("---")
    }

  }


  val myprog = new MyDSLProgram

  //myprog.myf()

  myprog.exportgraph()
  myprog.createMat()
  myprog.createScalaCode()
  myprog.createStagedScalaCode()
  myprog.createParamizedNonStagedScalaCode()
  myprog.createParamizedStagedScalarScalaCode()
  myprog.createParamizedStagedSplitComplexCode()
  myprog.createParamizedStagedInterleavedScalaCode()
}
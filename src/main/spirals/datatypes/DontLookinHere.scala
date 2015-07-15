package ch.ethz.spirals.datatypes

import ch.ethz.spirals.datatypes.DataTypeFactories.SplitComplexArray
import ch.ethz.spirals.datatypes.UnstagedImplicitOps._
import DataTypeFactories._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.datatypes.ElementOpsUnstaged.Complex

/**
 * Well - what do you expect form a trait called DontLookInHere?
 *
 * Welcome to implicit Hell!
 * Touch anywhere to break
 */


trait DontLookinHere extends StagedScala_Exp{
  self =>

  implicit val aops: ArrayOps[NoRep,Array,self.Rep,Double] = new self.ScalarSingleArrayOps[Double]
  implicit val nrep: NumericOps[self.Rep[Double]] =  new self.StagedNumericOps.NumericRepOps[Double]
  implicit val erep: ElementOps[Complex,self.Rep[Double]] = new ElementOpsUnstaged.ComplexOps[self.Rep[Double]]()
  implicit val aops1: ArrayOps[self.Rep,Vector,NoRep,Double] = new self.VectorArrayOps[Double]
  implicit val vrep1: LiftOps[self.Rep] = new self.LiftOpsRep()
  implicit val irep1: NumericOps[self.Rep[Int]] =  new self.StagedNumericOps.NumericRepOps[Int]


  implicit def exposeRepFromStagedComplex(implicit tag: Manifest[Double]): ExposeRep[StagedComplex] = new ExposeRep[StagedComplex]() {
    val freshExps = (u: Unit) => Vector(Arg[Double](tag), Arg[Double](tag))
    val vec2t = (v: Vector[Exp[_]]) => {
      val re: Exp[Double] = v(0).asInstanceOf[Exp[Double]] //RF: use the Manifest to make this safe
      val im: Exp[Double] = v(1).asInstanceOf[Exp[Double]]
      StagedComplex(re, im)
    }
    val t2vec = (c: StagedComplex) => Vector(c.re, c.im)
  }

  case class ComplexVector(val vec: Vector[StagedComplex])

  implicit def exposeRepFromVComplex(instance_size: Int)(implicit tag: Manifest[Double], exposeComplex: ExposeRep[StagedComplex]
    ): ExposeRep[ComplexVector] = new ExposeRep[ComplexVector]() {
    val freshExps = (u: Unit) => {
      val t = for (i <- 0 until instance_size) yield exposeComplex.freshExps()
      t.foldLeft(Vector.empty[Exp[_]])((acc, ele) => {
        acc ++ ele
      })
    }
    val vec2t: (Vector[Exp[_]] => ComplexVector) = (h: Vector[Exp[_]]) => ComplexVector(h.grouped(2).foldLeft(Vector.empty[StagedComplex])((acc, ele) => {
      acc :+ exposeComplex.vec2t(ele)
    }))
    val t2vec = (v: ComplexVector) => v.vec.foldLeft(Vector.empty[Exp[_]])((acc, ele) => {
      acc ++ exposeComplex.t2vec(ele)
    })
  }

  implicit def exposeRepFromScalarSplitComplex(size: Int)(implicit tag: Manifest[Double]): self.ExposeRep[CVector[NoRep, ElementOpsUnstaged.Complex,self.Rep,Double]]
  = new self.ExposeRep[CVector[NoRep, ElementOpsUnstaged.Complex,self.Rep,Double]] (){
    val freshExps: (Unit => Vector[self.Exp[_]]) = (u: Unit) => {
      val t = for(i <- 0 until size) yield Vector(self.Arg[Double](self.convertFromManifest(tag)), self.Arg[Double](self.convertFromManifest(tag)))
      t.foldLeft(Vector.empty[self.Exp[_]])((acc,ele) => { acc ++ ele })
    }
    val vec2t: (Vector[self.Exp[_]] => CVector[NoRep, ElementOpsUnstaged.Complex,self.Rep,Double]) = (v: Vector[self.Exp[_]]) => {
      val x = for (i <- 0 until size) yield{
        val re: self.Exp[Double] = v(i*2).asInstanceOf[self.Exp[Double]] //RF: use the Manifest to make this safe
        val im: self.Exp[Double] = v(i*2+1).asInstanceOf[self.Exp[Double]]
        Complex(re,im)
      }
      val t = new SplitComplexArray[NoRep, Array, self.Rep, Double](size)
      val r: CVector[NoRep, ElementOpsUnstaged.Complex,self.Rep,Double] = t.ini(x)
      r
    }
    val t2vec = (c: CVector[NoRep, ElementOpsUnstaged.Complex,self.Rep,Double]) => {
      val t = (for (i <- 0 until size) yield c(i)).toVector
      t.foldLeft(Vector.empty[self.Rep[_]]){(acc,ele) => acc ++ Vector(ele._re,ele._im)}
    }
  }
  implicit def exposeRepFromStagedInterleaved(size: Int)(implicit tag: Manifest[Vector[Double]]): self.ExposeRep[CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]]
  = new self.ExposeRep[CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]] (){
    val freshExps: (Unit => Vector[self.Exp[_]]) = (u: Unit) => {
      Vector(self.Arg[Vector[Double]](self.convertFromManifest(tag)))
    }
    val vec2t: (Vector[self.Exp[_]] => CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]) = (v: Vector[self.Exp[_]]) => {
      val inter: self.Exp[Vector[Double]] = v(0).asInstanceOf[self.Exp[Vector[Double]]] //RF: use the Manifest to make this safe
      val t = new InterleavedComplexArray[self.Rep, Vector, NoRep, Double](size,inter)
      t
    }
    val t2vec = (c: CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]) => {
      val t = (for (i <- 0 until size) yield c(c.vrep(i))).toVector
      t.foldLeft(Vector.empty[self.Rep[_]]){(acc,ele) => acc ++ Vector(ele._re,ele._im)}
    }
  }

  implicit def exposeRepFromSplitComplex(size: Int)(implicit tag: Manifest[Vector[Double]]): self.ExposeRep[CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]]
  = new self.ExposeRep[CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]] (){
    val freshExps: (Unit => Vector[self.Exp[_]]) = (u: Unit) => {
      Vector(self.Arg[Vector[Double]](self.convertFromManifest(tag)),self.Arg[Vector[Double]](self.convertFromManifest(tag)))
    }
    val vec2t: (Vector[self.Exp[_]] => CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]) = (v: Vector[self.Exp[_]]) => {
      val re: self.Exp[Vector[Double]] = v(0).asInstanceOf[self.Exp[Vector[Double]]] //RF: use the Manifest to make this safe
      val im: self.Exp[Vector[Double]] = v(1).asInstanceOf[self.Exp[Vector[Double]]] //RF: use the Manifest to make this safe
      val t = new SplitComplexArray[self.Rep, Vector, NoRep, Double](size,re,im)
      t
    }
    val t2vec = (c: CVector[self.Rep, ElementOpsUnstaged.Complex,NoRep,Double]) => {
      val t = (for (i <- 0 until size) yield c(c.vrep(i))).toVector
      t.foldLeft(Vector.empty[self.Rep[_]]){(acc,ele) => acc ++ Vector(ele._re,ele._im)}
    }
  }

}
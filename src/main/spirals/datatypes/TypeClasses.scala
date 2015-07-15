package ch.ethz.spirals.datatypes

import scala.collection.IndexedSeqLike
import scala.lms._
import scala.lms.ops._


/**
 * =========================================== NumericOps ==================================================
 *
 * NumericOps represent the numeric computations of the primitives. NumericOps require the inner type to
 * hold a Numeric type class.
 * In this context we define 3 distinct cases of numeric computations, on the primitives:
 *
 * 1. NumericNoRepOps  similar to Numeric, performs numeric computation on non-staged code. There is no
 * translation of this computations to any IR, and they are executed directly in Scala.
 * 2. NumericRepOps    represent SISD numeric computations that are translated in C-IR. The execution of this
 * code is delayed, and LMS kicks in.
 * 3. PackedNumericOps represent SIMD numeric computations that are translated into Intrinsics-IR. Similar to
 * NumericRepOps, code execution is staged and LMS kicks in. Note that it might make more
 * sense to move PackedNumericOps into VectorElementOps, however we like to have seamless
 * interface for the standard Numeric operations in both SIMD / SISD case.
 *
 * =========================================================================================================
 */

import scala.reflect.runtime.universe._


trait IdendityTypes{
  type NoRep[T] = T
  type Single[T] = T
}

object NumericOpsUnstaged extends IdendityTypes{

  class NumericNoRepOps[T: Manifest](implicit val numeric: Numeric[T]) extends NumericOps[NoRep[Single[T]]] {

    def plus(x: T, y: T) = numeric.plus(x,y)

    def minus(x: T, y: T) = numeric.minus(x,y)

    def times(x: T, y: T) = numeric.times(x,y)

    def fromInt(x: Int): T = numeric.fromInt(x)
  }
}

trait TypeClassesStagedNumericOps extends PureNumericOpsExpOpt with IdendityTypes{
  object StagedNumericOps {
    class NumericRepOps[T: Numeric : Manifest] extends NumericOps[Rep[T]] {
      def plus(x: Rep[T], y: Rep[T]) = numeric_plus[T](x, y)
      def minus(x: Rep[T], y: Rep[T]) = numeric_minus[T](x, y)
      def times(x: Rep[T], y: Rep[T]) = numeric_times[T](x, y)
      def fromInt(x: Int): Rep[T] =
      {
        val ev = implicitly[Numeric[T]]
        val b = ev.fromInt(x)
        Const(b)
      }
    }
  }
}

/**
 * =========================================== ElementOps ==================================================
 *
 * ElementOps represent higher order of computation that correspond to the mathematical notion of elements
 * represented in the IR. ElementOps require a Manifest, so it can be propagated to infer the Manifest of the
 * whole ElementOps type class. Furthermore they require a NumericOps case class, to handle the underlying
 * numerical operations on the primitives. In this case we deal with two different cases of Elements:
 *
 * 1. RealOps    that represent the operations of Real numbers, and
 * 2. ComplexOps that represent the numerical operation of Complex numbers
 *
 * =========================================================================================================
 */
object ElementOpsUnstaged {

  case class Real    [+T] (_re: T)
  case class Complex [+T] (_re: T, _im: T)

  class RealOps[T: Manifest : NumericOps] extends ElementOps[Real, T] {
    import numeric._
    def plus(x: Real[T], y: Real[T]) = Real(x._re + y._re)

    def minus(x: Real[T], y: Real[T]) = Real(x._re - y._re)

    def times(x: Real[T], y: Real[T]) = Real(x._re * y._re)
  }

  class ComplexOps[T: Manifest : NumericOps] extends ElementOps[Complex, T] {
    import numeric._

    def plus(x: Complex[T], y: Complex[T]) = Complex(x._re + y._re, x._im + y._im)

    def minus(x: Complex[T], y: Complex[T]) = Complex(x._re - y._re, x._im - y._im)

    def times(x: Complex[T], y: Complex[T]) = {
      val m1 = x._re * y._re
      val m2 = x._im * y._im
      val m3 = x._re * y._im
      val m4 = x._im * y._re
      Complex(m1 - m2, m3 + m4)
    }
  }
}

/**
 * ============================================ ArrayOps ===================================================
 *
 * ArrayOps define the data layout of the elements inside the higher-order CVector. The array operations are
 * closely related to the datatype of the CVector, and must have the knowledge of the type primitives of the
 * elements, as well as whether we are dealing with SIMD / SISD instructions. Each array operation is
 * translated into corresponding C-IR call, depending on whether the array is staged, scalar, SIMD or SISD
 * array. The ArrayOps carry Manifest of the primitive, to be able to propagate that information into the
 * underlying C-IR calls. ArrayOps come in 5 flavors:
 *
 * 1. StagedSingleArrayOps Underlying representation of the array, is a staged array. Apply / update methods
 * operate with SISD elements, which are staged. Ops are translated into pure C-IR.
 * 2. ScalarSingleArrayOps Underlying representation of the array is array of SISD elements which are staged.
 * Apply / update methods modify the array direcyly, without delay in code execution.
 * 3. StagedPackedArrayOps Underlying representation of the array is a staged array. Apply / update methods
 * create SIMD instructions that are represented into Intrinsics-IR.
 * 4. PackedScalarArrayOps Underlying representation of the array is a scalar array consisted of staged SIMD
 * data. Apply / update methods operate directly on the array, with no code delay.
 * 5. ScalarPackedArrayOps Underlying representation of the array is a scalar array consited of staged SISD
 * data. Apply / update methods translate to Intrinsics-IR instructions that convert
 * the staged SISD data into staged SIMD data.
 *
 * =========================================================================================================
 *
 */

object ArrayOpsUnstaged extends IdendityTypes{
  class UnstagedArray[T: Manifest]  extends ArrayOps[NoRep,Array,NoRep,T] {
    def alloc(s: NoRep[Int]): NoRep[Array[T]] = new Array[NoRep[T]](s)
    def apply(x: NoRep[Array[NoRep[T]]], i: NoRep[Int]): NoRep[T] = x.apply(i)
    //def update(x: NoRep[Array[T]], i: NoRep[Int], y: NoRep[T]) = x.update(i,y)
    def ini(from: Seq[NoRep[T]]): NoRep[Array[T]] = from.toArray
  }
}


trait TypeClassesStagedArrayOps extends PureNumericOpsExpOpt with VectorOpsExp with IdendityTypes{
  class ScalarSingleArrayOps[T: Manifest] extends ArrayOps[NoRep, Array, Rep, T] {
    def alloc(s: NoRep[Int]): NoRep[Array[Rep[T]]] = new Array[Rep[T]](s)
    def apply(x: NoRep[Array[Rep[T]]], i: NoRep[Int]): Rep[T] = x.apply(i)
    def ini(from: Seq[Rep[T]]): Array[Rep[T]] = from.toArray
  }

  class LiftOpsRep extends LiftOps[Rep] {
    def apply[T: Manifest](x: T) = Const(x)
  }

  class VectorArrayOps[T: Manifest] extends ArrayOps[Rep, Vector, NoRep, T] {
    def alloc(s: Rep[Int]): Rep[Vector[NoRep[T]]] = {
      ???
    }
    def apply(x: Rep[Vector[NoRep[T]]], i: Rep[Int]): Rep[NoRep[T]] = vector_apply(x,i)
    def ini(from: Seq[Rep[NoRep[T]]]): Rep[Vector[NoRep[T]]] = vector_obj_fromseq(from) //from.toArray
  }

  implicit def arrayofStaged[T:Manifest]: ArrayOps[NoRep,Array,Rep,T] = new ScalarSingleArrayOps[T]
}


/* ========================================================================================================= */
/* ============================================ LiftOps ==================================================== */
/* ========================================================================================================= */

object UnstagedLiftOps extends IdendityTypes{
  class LifOpsNoRep extends LiftOps[NoRep] {
    def apply[T: Manifest](x: T) = x
    def staged() = false
  }
}


object UnstagedImplicitOps extends IdendityTypes{
  /* =========================================== NumericOps ================================================== */

  implicit def numericNoRepOps[T: Numeric : Manifest]: NumericOps[NoRep[Single[T]]] = new NumericOpsUnstaged.NumericNoRepOps[T]

  /* =========================================== ElementOps ================================================== */

  implicit def complexOps[T: Manifest](implicit nops: NumericOps[T]): ElementOps[ElementOpsUnstaged.Complex, T] = new ElementOpsUnstaged.ComplexOps[T]()

  implicit def realOps[T: Manifest](implicit nops: NumericOps[T]): ElementOps[ElementOpsUnstaged.Real, T] = new ElementOpsUnstaged.RealOps[T]()

  /* ============================================ ArrayOps =================================================== */

  implicit def unstagedArrayOps[T:Manifest]: ArrayOps[NoRep,Array,NoRep,T]       = new ArrayOpsUnstaged.UnstagedArray[T]

  /* ========================================= VectorElementOps ============================================== */


  /* ============================================ LiftOps ==================================================== */

  implicit object NoRepObject extends UnstagedLiftOps.LifOpsNoRep

  /* ========================================================================================================= */
}

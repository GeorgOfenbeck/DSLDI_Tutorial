/**
 * _______  _______  ___   __      ____     Automatic
 * / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 * _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 * /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 * of DSP Algorithms
 * https://bitbucket.org/GeorgOfenbeck/spirals
 * SpiralS 0.1 Prototype - ETH Zurich
 * Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ch.ethz.spirals.datatypes

object DataTypeFactories {

  import ElementOpsUnstaged._


  class SplitComplexArray[V[_], A[_], R[_], T](s: Int, d1: V[A[R[T]]] = null, d2: V[A[R[T]]] = null)
                                              (implicit
                                               // Template operators
                                               aops: ArrayOps[V, A, R, T],
                                               vrep: LiftOps[V],
                                               erep: ElementOps[Complex, V[R[T]]],
                                               nrep: NumericOps[R[T]],
                                               irep: NumericOps[V[Int]]
                                                ) extends CVector[V, Complex, R, T] {

    private val two = irep.fromInt(2)
    private val data1: V[A[R[T]]] = if (d1 == null) aops.alloc(vrep(s)) else d1
    private val data2: V[A[R[T]]] = if (d2 == null) aops.alloc(vrep(s)) else d2

    def apply(i: V[Int]): Complex[V[R[T]]] = {
      new Complex(
        _re = aops.apply(data1, i),
        _im = aops.apply(data2, i)
      )
    }

    def ini(from: Seq[Complex[V[R[T]]]]) = {
      val v: Vector[Complex[V[R[T]]]] = from.toVector
      val first = v.map(e => e._re)
      val second = v.map(e => e._im)
      val d1 = aops.ini(first)
      val d2 = aops.ini(second)
      new SplitComplexArray[V, A, R, T](s, d1, d2)
    }

    def GT(A: CVector[V, Complex, R, T] => CVector[V, Complex, R, T],
           g: (Vector[Int]) => Int,
           s: (Vector[Int]) => Int,
           v: Vector[Int]
            )
    : CVector[V, Complex, R, T] => CVector[V, Complex, R, T] = (in: CVector[V, Complex, R, T]) => {
      //val out = in.create(in.size()) //create a same size element
      val size = in.size
      val out = new Array[Complex[V[R[T]]]](size)
      for (i <- 0 until size) out(i) = in(vrep(0))
      def helper(loopv: Vector[Int], currv: Vector[Int]): Unit = {
        if (loopv.tail.isEmpty) {
          //inner most loop
          val s0 = loopv.head
          val temp = new Array[Complex[V[R[T]]]](s0)
          for (i <- 0 until s0) temp(i) = in(vrep(0))
          //val int = in.create(s0)
          for (i <- 0 until s0) {
            //val is: V[Int] = irep.fromInt(i)
            val newv: Vector[Int] = i +: currv
            val idx = g(newv)
            val ele = in.apply(vrep(idx))
            temp.update(i, ele)
            //int.update(is,ele)
          }
          val intvec = in.ini(temp)
          val outt = A(intvec)
          for (i <- 0 until s0) {
            //val is: V[Int] = irep.fromInt(i)
            val newv: Vector[Int] = i +: currv
            val idx = s(newv)
            val ele = outt.apply(vrep(i))
            out.update(idx, ele)
            //out.update(idx,ele)
          }
        }
        else {
          val sk = loopv.head
          val rest = loopv.tail
          for (i <- 0 until sk) {
            //val is: V[Int] = irep.fromInt(i)
            helper(rest, i +: currv)
          }
        }
      }
      helper(v, Vector.empty)
      val outvec = in.ini(out)
      outvec
    }

    def size() = s
  }

  class InterleavedComplexArray[V[_], A[_], R[_], T](s: Int, d: V[A[R[T]]])
                                                    (implicit
                                                     // Template operators
                                                     aops: ArrayOps[V, A, R, T],
                                                     vrep: LiftOps[V],
                                                     erep: ElementOps[Complex, V[R[T]]],
                                                     nrep: NumericOps[R[T]],
                                                     irep: NumericOps[V[Int]]
                                                      ) extends CVector[V, Complex, R, T] {

    private val two = irep.fromInt(2)
    private val data: V[A[R[T]]] = if (d == null) aops.alloc(vrep(s)) else d


    def apply(i: V[Int]): Complex[V[R[T]]] = {
      new Complex(
        _re = aops.apply(data, irep.times(i, vrep(2))),
        _im = aops.apply(data, irep.plus(irep.times(i, vrep(2)), vrep(1)))
      )
    }

    def ini(from: Seq[Complex[V[R[T]]]]) = {
      val v: Vector[Complex[V[R[T]]]] = from.toVector
      val inter = v.flatMap(e => Vector(e._re, e._im))
      new InterleavedComplexArray[V, A, R, T](s, aops.ini(inter))
    }

    def GT(A: CVector[V, Complex, R, T] => CVector[V, Complex, R, T],
           g: (Vector[Int]) => Int,
           s: (Vector[Int]) => Int,
           v: Vector[Int]
            )
    : CVector[V, Complex, R, T] => CVector[V, Complex, R, T] = (in: CVector[V, Complex, R, T]) => {
      //val out = in.create(in.size()) //create a same size element
      val size = in.size
      val out = new Array[Complex[V[R[T]]]](size)
      for (i <- 0 until size) out(i) = in(vrep(0))
      def helper(loopv: Vector[Int], currv: Vector[Int]): Unit = {
        if (loopv.tail.isEmpty) {
          //inner most loop
          val s0 = loopv.head
          val temp = new Array[Complex[V[R[T]]]](s0)
          for (i <- 0 until s0) temp(i) = in(vrep(0))
          //val int = in.create(s0)
          for (i <- 0 until s0) {
            //val is: V[Int] = irep.fromInt(i)
            val newv: Vector[Int] = i +: currv
            val idx = g(newv)
            val ele = in.apply(vrep(idx))
            temp.update(i, ele)
            //int.update(is,ele)
          }
          val intvec = in.ini(temp)
          val outt = A(intvec)
          for (i <- 0 until s0) {
            //val is: V[Int] = irep.fromInt(i)
            val newv: Vector[Int] = i +: currv
            val idx = s(newv)
            val ele = outt.apply(vrep(i))
            out.update(idx, ele)
            //out.update(idx,ele)
          }
        }
        else {
          val sk = loopv.head
          val rest = loopv.tail
          for (i <- 0 until sk) {
            //val is: V[Int] = irep.fromInt(i)
            helper(rest, i +: currv)
          }
        }
      }
      helper(v, Vector.empty)
      val outvec = in.ini(out)
      outvec
    }

    def size() = s
  }


}




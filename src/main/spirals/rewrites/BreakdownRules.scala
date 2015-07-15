/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */
package ch.ethz.spirals.rewrites


import scala.math._
import ch.ethz.spirals.dsls._

import org.scalacheck._
import org.scalacheck.Gen._
import org.scalacheck.Prop._


//-------------------------------------------------------------------------------
//Data Structures used to store the nodes of the RuleTree

//after 1st application:
// BreakDown ( DFT(x),
//               rule: DFT_CT
//               rule2spl: f
//               children: DFT(x1), DFT(x2)
//           )
//               where DFT(x1) and DFT(x2) look like
//
//            BreakDown( DFT(x1), None)

//The idea is that the Option Type is replace during the search (copy)

//The final node should like like this:
// BreakDown (DFT(2),
//              rule: DFT_Base
//              rule2spl: f (just take child)
//              children: F2

//-----------------------------------------------------------------------------
//this is used once its decided which breakdown to use (rule/children)
class BreakDown_resolved (
                           val rule : PartialFunction[SPL,List[BreakDown]],
                           val children : List[BreakDown]
                           )
//this stores the non terminal and possibly the rule that is applied

class BreakDown(
                 val nt: SPL,
                 val applied_bd: Option[BreakDown_resolved]
                 )
{
  def this(nt: SPL) = this(nt,None)

  override def toString =
  {
    //=================================================================
    def PrintRuleTree(in_bd : BreakDown): String = {
      var outstring = ""
      val rule_names = List("WHT_CT", "WHT_Base")
      printrecurse(in_bd,0)
      def printrecurse(in_bd: BreakDown, level: Int)
      {
        for (i <- 0 until level){
          outstring = outstring + " "
        }
        outstring = outstring + in_bd.nt.toString +"\n"
        in_bd.applied_bd map (bd_applied =>
          {
            for (i <- 0 until level)
              outstring = outstring + " "
            outstring = outstring + rule_names(BreakdownRules.all.indexOf(bd_applied.rule)) + "\n"

          bd_applied.children map ( child => printrecurse(child,level+1))
          }
          )

      }
      outstring
    }
    PrintRuleTree(this)
  }

}




object BreakdownRules {

  // WHT_CT: 1965
  //   General Cooley-Tukey Rule
  //   DFT_n = (DFT_n/d tensor I_d) * diag * (I_n/d tensor F_d) * perm
  //
  // Cooley/Tukey:
  //   An Algorithm for the Machine Calculation of Complex Fourier Series.
  //   Mathematics of Computation, Vol. 19, 1965, pp. 297--301.


  val WHT_CT : PartialFunction[SPL,List[BreakDown]] =
  {
    case (WHT(n))
      if (n > 2 && n % 2 == 0) => //this is the guard
    {
      //create Breakdown option with uninitialized children (no breakdown defined yet - only Non Terminal
      MathUtilities.DivisorPairs(n).map(pair=> new BreakDown(WHT(n),
        Some( new BreakDown_resolved(
          WHT_CT,
          //get_rule(pair),
          List(
            new BreakDown(WHT(pair(0))),
            new BreakDown(WHT(pair(1)))
          )))))
    }
  }

  //The final node should like like this:
  // BreakDown (DFT(2),
  //              rule: DFT_Base
  //              rule2spl: f (just take child)
  //              children: F2

  val WHT_Base : PartialFunction[SPL,List[BreakDown]] =
  {
    case (WHT(n))
      if (n == 2) => //this is the guard
    {
      List(new BreakDown(WHT(n), Some( new BreakDown_resolved(
        WHT_Base,List()
      ))
      ))
    }
  }

  val all = List(WHT_CT, WHT_Base)


  //-------------------------------------------------------------------------------
  // Random application of breakdownrules through scalacheck



  def  genRandomWHTRuleTree(whtsize: Int): Gen[BreakDown] = for {
    size <- whtsize
    bd <- genRandomRuleTree(WHT(size))
  } yield bd


  def genRandomWHTRuleTree(): Gen[BreakDown] = for {
    //size <- Math.pow(2,23).toInt
    size <- oneOf(4,8,16,32,64,128,256,512) //only 2 powers for WHT
    bd <- genRandomRuleTree(WHT(size))
  } yield bd

  def genRandomBreakDown(in_spl: SPL): Gen[BreakDown] = for {
    rule <- genRules(in_spl)
    df <- genBreakDown(in_spl, rule)
  } yield df

  def genRandomBreakDown(in_bd: BreakDown): Gen[BreakDown] = genRandomBreakDown(in_bd.nt)

  def genRules (in_spl: SPL) : Gen[PartialFunction[SPL,List[BreakDown]]] = for {
    r <- oneOf(BreakdownRules.all filter (_.isDefinedAt(in_spl)))
  } yield r

  def genBreakDown (in_spl: SPL, rule: PartialFunction[SPL,List[BreakDown]]): Gen[BreakDown] = for {
    df <- oneOf(rule.apply(in_spl))
  } yield df

  def getChildren(in: BreakDown): List[BreakDown] = in.applied_bd.map ( resolved => resolved.children).getOrElse(List())

  def expandChildren (in: List[BreakDown]): Gen[List[BreakDown]] = {
    implicit def buildableList[T] = new util.Buildable[T,List[T]] {
      def builder = new scala.collection.mutable.ListBuffer[T]
    }
    val t = in.map( child => genRandomRuleTree(child.nt))
    val listnow: Gen[List[BreakDown]] = Gen.sequence(t) //(util.Buildable.buildableList)
    listnow
  }

  def genChildren(in_resolved: BreakDown_resolved): Gen[List[BreakDown]] = for {
    l <- in_resolved.children
  } yield l

  def genBreakDown_resolved(in_bd: BreakDown): Gen[BreakDown_resolved] = for {
    rule <- genRules(in_bd.nt)
    df <- genBreakDown(in_bd.nt, rule)
  } yield new BreakDown_resolved(rule,df.applied_bd.get.children)

  def genRandomRuleTree (in_spl: SPL): Gen[BreakDown] = {
    if (in_spl == F_2()) {
      new BreakDown(in_spl, None)
    }
    else
    {
      genRandomRuleTree(new BreakDown(in_spl,None))
    }
  }

  def genRandomRuleTree (in_spl: BreakDown): Gen[BreakDown] = for {
    onelevel <- genRandomBreakDown(in_spl)
    resolved_part <- genBreakDown_resolved(onelevel)
    children <- genChildren(resolved_part)
    expanded <- expandChildren(children)
  } yield new BreakDown(
      in_spl.nt,
      Some(
        new BreakDown_resolved(
          resolved_part.rule,
          expanded
        )
      )
    )



}



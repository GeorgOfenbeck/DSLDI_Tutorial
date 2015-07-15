package ch.ethz.spirals.dsls
import scala.lms._
import scala.lms.internal.InternalFunctionsExp

trait SPL_Exp extends BaseExp with InternalFunctionsExp {

  //This part defines how to implicitly create staged versions out of regular SPL objects
  //implicit def SPLtoExp(i: SPL): Exp[SPL] = unit(i)
  //-----------------------------------------------------------------------
  //Our DSL
  implicit def SPLtoExp(i: SPL): Exp[SPL] = unit(i)
  implicit def mkSPLExpOps(lhs: Exp[SPL]): SPLOps = new SPLOps(lhs)


  class SPLOps(x: Exp[SPL]) {
    def tensor(y: Exp[SPL]): Exp[SPL] = Tensor(x, y, createTensorSize(x,y))
    def compose(y: Exp[SPL]): Exp[SPL] = Compose(x, y, createComposeSize(x,y))
  }
  class SPLDef(val size: Int) extends Def[SPL] //we create a special definition for SPL which contains the size
  case class Tensor(x: Exp[SPL], y: Exp[SPL], override val size: Int) extends SPLDef(size)
  case class Compose(x: Exp[SPL], y: Exp[SPL], override val size: Int) extends SPLDef(size)

  //-----------------------------------------------------------------------
  //This part deals with getting size info SPL Nodes (can be also the composition of nodes
  //thats why we need to fetch this information from the sea of nodes rather then just from the two elements
  def getSPLSize(x: Exp[SPL]): Int = {
    val otp = getTP(x) //look into the sea of nodes and fetch info for symbol x
    val rhs = otp.map(t => t.rhs) //we only care about the "Definition" it carries
    rhs match {
      case Some(ConstDef(spl: SPL)) => spl.size
      case Some(spl: SPLDef) => spl.size //return the size it stores
      case Some(_) => { assert(false, "Seems we are trying to extract SPL size info from a node that is not an SPL object - namely: " + otp); ???}
      case _ => { assert(false, "apparently the Exp is not part of the internal IR - how did you manage this?"); ???}
    }
  }
  def createTensorSize(x: Exp[SPL], y: Exp[SPL]): Int = getSPLSize(x) * getSPLSize(y)
  def createComposeSize(x: Exp[SPL], y: Exp[SPL]): Int = getSPLSize(x)
}

class SPL_DSL extends SPL_Exp
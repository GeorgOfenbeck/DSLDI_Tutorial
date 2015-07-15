package ch.ethz.spirals.rewrites
import ch.ethz.spirals.dsls._
import scala.lms._
import internal._

case class MyComplex(re: Double, im: Double){
  def +(that: MyComplex) = MyComplex(this.re + that.re, this.im + that.im)
  def -(that: MyComplex) = MyComplex(this.re - that.re, this.im - that.im)
}
  
trait SPL_DSL2Scala extends Emit[Map[Int, Vector[MyComplex] => Vector[MyComplex]]] {
  self =>
  val IR: SPL_Exp with InternalFunctionsExp
  import IR._

  override def emitNode(tp: self.IR.TP[_], fmap: Map[Int, Vector[MyComplex] => Vector[MyComplex]],
                        block_callback: (self.IR.Block,Map[Int, Vector[MyComplex] => Vector[MyComplex]]) => Map[Int, Vector[MyComplex] => Vector[MyComplex]])
  : Map[Int, Vector[MyComplex] => Vector[MyComplex]] = {        
      tp.rhs match{
        //--------------------------------Compose -----------------------------
        case Compose(Exp(a),Exp(b),size) => {
          ???
        }
        //-------------------------------Tensor--------------------------------
        case Tensor(Exp(a),Exp(b),size) => {
          val f: Vector[MyComplex] => Vector[MyComplex] = (id2tp(a).rhs,id2tp(b).rhs) match {
            case (ConstDef(I(n)),_) => (in: Vector[MyComplex]) =>
              ???
            case (_,ConstDef(I(n))) => (in: Vector[MyComplex]) =>
              ???
            case _ => ??? //we dont support anything else for this tutorial
          }
          fmap + (tp.sym.id -> f)
        }
        //-------------------------------SPl Objects--------------------------------
        case ConstDef(x: SPL) => {
          val f: Vector[MyComplex] => Vector[MyComplex] =
            x match{
              case I(n) => ( (in: Vector[MyComplex]) => in )
              case F_2() => ???

              case _ => ??? //we dont support anyting else for this tutorial
          }
          fmap + (tp.sym.id -> f)
        }
        //------------------------------default traversal-----------------------------------------
        case InternalLambda(_,_,block,_,_) => block_callback(block,fmap)
        case _ => super.emitNode(tp,fmap,block_callback)
      }
    }
  }


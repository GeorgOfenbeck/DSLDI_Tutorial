package scala.lms
package internal


trait Blocks extends Expressions {

 case class Block(val res: Vector[Exp[_]])  {
  require(res.isEmpty == false)
 }

 def blocks(e: Any): Vector[Block] = e match {
  case b: Block => Vector(b)
  case p: Product => p.productIterator.toVector.flatMap(blocks(_))
  case _ => Vector.empty
 }
}


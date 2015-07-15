package scala.lms.ops

trait VectorOps extends ImplicitOps{
  object SVector {
    def apply[T:Manifest](xs: Rep[T]*) = vector_obj_fromseq(xs)
    def apply[T:Manifest](xs: List[Rep[T]]) = vector_obj_fromseq(xs)
  }

  def vector_obj_fromseq[T:Manifest](xs: Seq[Rep[T]]): Rep[Vector[T]]
  def vector_apply[T:Manifest](x: Rep[Vector[T]], n: Rep[Int]): Rep[T]
}


trait VectorOpsExp extends VectorOps with ImplicitOpsExp{
  case class VectorFromSeq[T:Manifest](xs: Seq[Exp[T]]) extends Def[Vector[T]]
  case class VectorApply[T:Manifest](a: Exp[Vector[T]], n: Exp[Int]) extends Def[T]
  def vector_obj_fromseq[T:Manifest](xs: Seq[Exp[T]]) = VectorFromSeq(xs)
  def vector_apply[T:Manifest](x: Exp[Vector[T]], n: Exp[Int]): Exp[T] = VectorApply(x, n)
}
package scala.lms

/**
 * Component for abstraction over the run-time representation of types. The `TypeRep` abstraction
 * can carry additional information with the run-time type (e.g. bit width for hardware representation).
 *
 * NOTE: Parametric types must be lifted explicitly since compiler does not generate
 * TypeRep[X[T]] if implict TypeRep[T] is in scope. For example, @see LiftArrayType.
 */
trait TypeRepBase {
 trait TypeRep[T] {
  def mf: Manifest[T]

  def typeArguments: List[Manifest[_]]
  def arrayManifest: Manifest[Array[T]]
  def runtimeClass: java.lang.Class[_]
  def erasure: java.lang.Class[_]
  def <:<(that: TypeRep[_]): Boolean
 }

 case class TypeExp[T](mf: Manifest[T]) extends TypeRep[T] {
  def typeArguments: List[Manifest[_]]   = mf.typeArguments
  def arrayManifest: Manifest[Array[T]] = mf.arrayManifest
  def runtimeClass: java.lang.Class[_] = mf.runtimeClass
  def <:<(that: TypeRep[_]): Boolean = mf.<:<(that.mf)
  def erasure: java.lang.Class[_] = mf.erasure
  override def canEqual(that: Any): Boolean = mf.canEqual(that)
  override def equals(that: Any): Boolean = mf.equals(that)
  override def hashCode = mf.hashCode
  override def toString = mf.toString
 }

 def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T] = tr
 implicit def typeRepFromManifest[T](implicit mf: Manifest[T]): TypeRep[T] = TypeExp(mf)
 implicit def convertFromManifest[T](mf: Manifest[T]): TypeRep[T] = TypeExp(mf)
}


/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1
 */
trait Base extends TypeRepBase {
 type API <: Base
 type Rep[T]
 protected def unit[T:TypeRep](x: T): Rep[T]            //TODO - why protected??

 // always lift Unit and Null (for now)
 implicit def unitToRepUnit(x: Unit) = unit(x)
 implicit def nullToRepNull(x: Null) = unit(x)
 def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T]
}



import scala.lms.internal._
/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with InternalFunctions with Blocks { //with Effects{
  type Rep[T] = Exp[T]
  def unit[T:TypeRep](x: T) = Const(x)


  implicit def exposeRepFromRep[T](implicit tag: TypeRep[T]): ExposeRep[Rep[T]] = new ExposeRep[Exp[T]](){
    val freshExps = (u: Unit) => Vector(Arg[T](tag))
    val vec2t: Vector[Exp[_]] => Exp[T] = (v: Vector[Exp[_]]) => v.head.asInstanceOf[Exp[T]] //TODO: Horrible cast - get rid of it
    val t2vec: Exp[T] => Vector[Exp[T]] = (x: Rep[T]) => Vector(x)
  }
}
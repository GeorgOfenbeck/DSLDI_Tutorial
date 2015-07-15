package scala.lms
package targets

import internal._
import scala.reflect._

trait GenericCodegen extends Emit[String]{
  self =>
  val IR: BaseExp with InternalFunctionsExp
  import IR._

  def emitDataStructures(): String = {""}


  override def emitc(start: String,
                     it: Iterator[self.IR.TP[_]],
                     block_callback: (self.IR.Block,String) => String ): String = {
    it.foldLeft(start){
      (acc,ele) => {
        val t: String = emitNode(ele,acc,block_callback)
        acc + t
      }
    }
  }

  //
  def getBlockResults[A](s: Block): Vector[Exp[_]] = s match {
    case Block(x) => x
  }



  def quote(x: Exp[_]) : String = {
    val tp: TP[_] = id2tp(x.id)
    quote(tp)
  }

  def quote(x: TP[_]) : String = x.sym match {
    case Const(s: String) => "\""+s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(c: Char) => "'"+(""+c).replace("'", "\\'").replace("\n", "\\n")+"'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(i: Int) => i.toString
    case Const(null) => "null"
    case Const(z) => z.toString
    case Exp(n) => "x"+n
    case _ => throw new RuntimeException("could not quote " + x)
  }


  // Provides automatic quoting and remapping in the gen string interpolater
  implicit class CodegenHelper(sc: StringContext) {
    def printToStream(arg: Any): String = {
      quoteOrRemap(arg)
    }

    def quoteOrRemap(arg: Any): String = arg match {
      case xs: Seq[_] => xs.map(quoteOrRemap).mkString(",")
      //case tp: TP[_] => quote(tp)
      case exp: Exp[_] => quote(exp)
      case m: Manifest[_] => remap(m)
      case s: String => s
      case _ => throw new RuntimeException(s"Could not quote or remap $arg")
    }

    // First line of a part of the context may contain
    // a | and therefore should not be stripped
    def stripContextPart(part: String): String = {
      val lines = part.linesWithSeparators
      if (!lines.hasNext) part
      else lines.next + (lines.foldLeft("")(_+_).stripMargin)
    }

    def src(args: Any*): String = {
      sc.raw(args.map(quoteOrRemap): _*).stripMargin
    }

    def gen(args: Any*): String = {
      sc.checkLengths(args)
      val start :: contextStrings = sc.parts.iterator.toList
      val s0 = start.stripMargin

      (args zip contextStrings).foldLeft(s0){
        (acc,ele) => {
          val (arg,contextString) = ele
          acc + arg + stripContextPart(contextString)
        }
      }
    }
  }


  // optional type remapping (default is identity)
  def remap(s: String): String = s
  def remap[A](s: String, method: String, t: Manifest[A]) : String = remap(s, method, t.toString)
  def remap(s: String, method: String, t: String) : String = s + method + "[" + remap(t) + "]"
  def remap[A](m: Manifest[A]): String = m match {
     //TODO: GO -> check with Vojin / Tiark - this got lost in the macro-trans - do we care?
    //case rm: RefinedManifest[A] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
    case _ if m.erasure == classOf[Variable[Any]] =>
      remap(m.typeArguments.head)
    case _ =>
      // call remap on all type arguments
      val targs = m.typeArguments
      if (targs.length > 0) {
        val ms = m.toString
        ms.take(ms.indexOf("[")+1) + targs.map(tp => remap(tp)).mkString(", ") + "]"
      }
      else m.toString
  }
  def remapImpl[A](m: Manifest[A]): String = remap(m)


}

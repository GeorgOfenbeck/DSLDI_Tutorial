package scala.lms
package targets
package graphviz

import scala.lms.internal._


trait GraphVizExport {
 self =>
 val IR: BaseExp with InternalFunctionsExp

 type specCM = CodeMotion {
  val reifiedIR: ReificationPure {
   val IR: self.IR.type
  }}

 def emitDepGraph(file: String, landscape: Boolean = false) : String = ???

 def quote(x: Any) = "\""+x+"\""

 def emitDepGraphf[A,R]( f: Function1[A,R])(implicit args: IR.ExposeRep[A], returns: IR.ExposeRep[R]): (String,specCM) = {
  val reify = new ReifyPure {
   override val IR: self.IR.type = self.IR
  }
  val reification = reify.reifyProgram(f)(args, returns)
  val cm: specCM = CodeMotion(reification)
  (emitDepGraph(cm),cm)
 }



 def emitDepGraph(cm: specCM): String = {


  def emitNode(node: cm.EnrichedGraphNode): String = {
   val tp = cm.reifiedIR.id2tp(node.irdef)
   val nodestring = tp.sym.id + " [label=" + quote(tp.sym.id + " \\n " + tp.rhs) +
   "\n,shape=box]"

   val sucessorstring = node.successors.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"s\"] "
   }
   val predecessorstring = node.predecessors.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"p\"] "
   }

   val blockresids: Set[Int] = node.blocks.flatMap(v => v.res.map(res => res.id))
   val blockdepstring = blockresids.foldLeft(""){
    (acc,ele) => acc + "\n\"" + node.irdef + "\" -> \"" + ele + "\"" + " [label=\"b\"] "
   }


   nodestring + sucessorstring + predecessorstring + blockdepstring

  }
  //we emit the head node and all blocks
  //this is assuming that head node is always a function and everything else is contained within
  val lamdbaid: Int = cm.block_cache.root.sym.id
  val head = emitNode(cm.enriched_graph(lamdbaid))


   val graphstring = cm.block_cache.blockinfo.foldLeft(Vector.empty[String])( (acc,ele) => {
     val (block, blockinfo) = ele
     val blockres = blockinfo.children.foldLeft(Vector.empty[String]) {
      (iacc,iele) => {
       val (id, node) = iele
       iacc :+ emitNode(node)
      }
     }
     acc ++ blockres
    })


  "digraph G {\n" + head + "\n" + graphstring.mkString("\n") + "\n}"
 }












}

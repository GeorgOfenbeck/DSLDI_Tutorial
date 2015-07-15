package scala.lms
package internal

/**
 * This is not stable code!
 */

/*
  The general assumption of the new Codemotion is, that there cannot be a Sym without a corresponding TP! (which was different in previous LMS versions)
 */


trait CodeMotion {
  val reifiedIR: ReificationPure
  import reifiedIR.IR._


  /**
   * @param irdef ID of the TP
   * @param predecessors Set of Nodes that we depend on (IDs)
   * @param successors Set of Nodes that depend on this node (IDs)
   * @param bounds Symbols bound under the current TP
   * @param blocks The IDs of symbols bound within Blocks of this Node (e.g. (Block{ Vector(Sym(4)}, Block{ Vector(Sym(4),Sym(3)} - we would save 3,4)
   */
  case class EnrichedGraphNode( irdef: Int, predecessors: Set[Int], successors: Set[Int], bounds: Set[Int], blocks: Set[Block] )

  /**
   * @param children The children of the current block (if the block contains nested blocks, the nested children will be saved within the sub block)
   * @param child_schedule One possible topological sort that was used during code motion
   * @param uplinks All Nodes used from outside this block
   * @param roots All Nodes without predecessor in the current Block
   */
  case class BlockInfo(children: Map[Int,EnrichedGraphNode], child_schedule: Vector[Int], uplinks: Set[Int], roots: Set[Int])

  /**
   * @param root
   * @param blockinfo
   */
  case class IRBlockInfo(val root: TP[_], val blockinfo: Map[Block,BlockInfo]){
    def getHead(): BlockInfo = {
      root.rhs match {
        case InternalLambda(f,x,y,args,returns) => blockinfo(y)
        case _ => assert(false, "a IRBlockInfo was created that did not have a Internal Lambda as root"); ???
      }

    }
  }

  /**
   * An Intmap with ID -> EnchancedNode for all TPs
   * see enhanceDAG() for more info
   **/
  lazy val enriched_graph = enhanceDAG()

  /**
   * used to build block_cache - should only be used internal (mutable)
   */
  protected var bcache = Map.empty[Block,BlockInfo]


  lazy val block_cache: IRBlockInfo = {
    bcache = Map.empty[Block,BlockInfo] //just in case someone initialized that by accident before
    getBlockInfo(reifiedIR.rootlambda.y)
  }


  /**
    * @param block The block on which code motion should be performed - should always be the result block of the reified DAG
    * @tparam A
    */

  /**
   * This will traverse the DAG in topological order (result to input direction) and create the BlockInfo data structure
   * To do so it will recurse into all blocks and bind symbols to blocks
   * It will also note all symbols within blocks that are referencing blocks higher up in the nesting
   * After this function is executed bcache will be filled with a BlockInfo data structure per block that appears in the DAG
   * @param block The bottom most symbol of the graph which should always be the block contained in the root lambda
   * @return A Hashmap of Block -> BlockInfo which also contains info about the root block
   */
  protected def getBlockInfo(block: Block): IRBlockInfo = {
    val res = block.res

    if (true) {//TODO - RF!
      //if (!bcache.contains(resid)) { //this should always be true
      /*if (block.res.size > 1)
        assert(false, "fix me")*/
      val  (mark,pmark,stack,rscope,rfullscope,uplinks,roots) = //calling visited_nested with the empty status variables and the full graph to start things off
        res.foldLeft( (Set.empty[Int],Set.empty[Int],Vector.empty[Int],enriched_graph,enriched_graph,Set.empty[Int],Set.empty[Int]) ) {
          (acc,ele) =>  {
            val blocksym = ele.id
            val focused = enriched_graph(blocksym)
            val rootids = block.res.map(r => r.id).toSet
            val children = depGraph(rootids,rootids ++ focused.bounds,Map.empty[Int,EnrichedGraphNode],enriched_graph) //get the subgraph that depends on that bounds
            val childrenwithroot = children + (blocksym -> focused)
            val  (mark,pmark,stack,rscope,rfullscope,uplinks,roots) = visit_nested(ele.id,acc._1,acc._2,acc._3,acc._4,acc._5,acc._6 - ele.id,acc._7)
            val cache_entry: (Block, BlockInfo) = (block,BlockInfo(childrenwithroot,stack,uplinks,roots))
            bcache = bcache + cache_entry
            (mark,pmark,stack,rscope,rfullscope,uplinks,roots)
          }
        }

      //val cache_entry: (Block, BlockInfo) = (block,BlockInfo(rfullscope,stack,uplinks,roots))

    }
    else{
      assert(false,"in the current setup getBlockInfo should only be callable from the root block - therefore we should" +
        "never end up here!")
    }
    assert(bcache.contains(block),"sanity check fails?")
    IRBlockInfo(reifiedIR.def2tp(reifiedIR.rootlambda),bcache)
  }

  /**
   *
   * @param n The current node (index into globaldefs therefore also enriched_graph)
   * @param tmark A set of nodes that we are currently working on (and therefore should not see while exploring the rest of the graph)
   * @param pmark The set of nodes already processed
   * @param sort One possible topological sort of the current graph (the one used during CM)
   * @param curr_scope The subset of nodes that are available in the current scope - at the start this is the whole DAG and shrinks with block traversed
   * @param full_scope The whole Dag (index into globaldefs -> EnrichedNode)
   * @param uplinks All symbols that are predecessors, but are not in the current scope
   * @param roots All symbols that do not have a predecessor in the current scope (might have in a higher nest)
   * @return
   */
  protected def visit_nested (n: Int, tmark: Set[Int], pmark: Set[Int], sort: Vector[Int], curr_scope: Map[Int,EnrichedGraphNode], full_scope: Map[Int,EnrichedGraphNode],
                              uplinks: Set[Int], roots: Set[Int]): (Set[Int], Set[Int], Vector[Int], Map[Int,EnrichedGraphNode], Map[Int, EnrichedGraphNode], Set[Int], Set[Int])  = {
    if (!curr_scope.contains(n)) {
      return (tmark,pmark,sort, curr_scope, full_scope, uplinks + n, roots) //add n to uplinks
    }
    if (tmark.contains(n))  //if n has a temporary mark then stop (not a DAG)
      assert(false,"not a dag")

    val newtmark = tmark + n //mark n temporarily

    if (pmark.contains(n)){ //we already did that node - so return
      return (tmark,pmark,sort, curr_scope, full_scope, uplinks, roots)
    }

    else {
      val focused: EnrichedGraphNode = curr_scope(n)
      val next = focused.predecessors //.filter(x => full_scope(x).irdef.isDefined) //all nexts

      val (allnext, curr_nscope, full_nscope) =
        if (!focused.blocks.isEmpty) {//we have a block
          if (focused.blocks.size > 1)
            assert(false, "implement this!")
        val (curr_uscope, full_uscope): (Map[Int,EnrichedGraphNode],Map[Int,EnrichedGraphNode]) =
          if (!bcache.contains(focused.blocks.head)) {

            update_nest(focused.blocks.head,focused.bounds, curr_scope, full_scope)
          }
          else
            (curr_scope, full_scope) //we update the scope to have the blockinfo

          val nfocused = focused //curr_uscope(n) //refresh the focused
          if (!bcache.contains(focused.blocks.head)) assert(false, "this should just not happen")
          val binfo = bcache(focused.blocks.head)
          (binfo.uplinks ++ next, curr_uscope, full_uscope)
        }
        else {
          (next,curr_scope, full_scope)
        }

      val newroots =
        if (allnext.filter(x => curr_scope.contains(x)).isEmpty) //this node is one of the root symbols within the block
          roots + n
        else
          roots



      val (rtmark,rpmark,rsort,rcurrscope,rfullscope,ruplinks,rroots) =
        allnext.foldLeft((newtmark,pmark,sort,curr_nscope, full_nscope,uplinks,newroots)){
          (acc,ele) => visit_nested(ele,acc._1,acc._2,acc._3,acc._4,acc._5,acc._6 - ele,acc._7) }

      val newpmark = rpmark + n
      val newstack = rsort :+ n
      (tmark,newpmark,newstack,rcurrscope,rfullscope,ruplinks, rroots)
    }
  }

  /**
   * This is called by visit_nested. Will update the given block (blocksym) in the bache by calling in turn visit_nested
   * on the subgraph that is bound within the block
   *
   * @param blocksym The Block Symbol (e.g. Block(Sym(4)) -> 4) for which we want to create a new bcache entry
   * @param curr The current scope - this shrinks with every node already visited in the process
   * @param full The full scope of the DAG - stays full always
   * @return Returns the updated (curr,full) tuple
   */
  protected def update_nest(blockhead: Block, boundsyms: Set[Int], curr: Map[Int,EnrichedGraphNode], full: Map[Int,EnrichedGraphNode]): (Map[Int,EnrichedGraphNode], Map[Int,EnrichedGraphNode])  = {
    //val focused = curr(blocksym)
    //val blockhead = focused.blocks.head
    //if (!focused.blocks.tail.isEmpty) assert(false, "we are not handling this yet (multiple blocks in one symbol")
    val blockrestail = blockhead.res.tail.map(r => r.id).toSet
    val roots = blockhead.res.map(r => r.id).toSet
    val children = depGraph(roots,roots ++ boundsyms,Map.empty[Int,EnrichedGraphNode],full) //get the subgraph that depends on that bounds
    //val centry: (Int, EnrichedGraphNode)  = (blockhead,focused)
    //val children = children_dep + centry //depgraph doesnt include the root
    val  (mark,pmark,stack,rcurrscope,rfullscope,uplinks,rroots) =
      blockhead.res.foldLeft(Set.empty[Int],Set.empty[Int],Vector.empty[Int],curr, full,Set.empty[Int],Set.empty[Int]){
        (acc,ele) => visit_nested(ele.id,acc._1,acc._2,acc._3,acc._4,acc._5,acc._6 - ele.id,acc._7)
      }


    //val childrenwithroot = children + (root -> enriched_graph(root))
    val binfo = BlockInfo(children,stack,uplinks,rroots)
    //val binfo = BlockInfo(children,stack,uplinks,roots)
    val cache_entry: (Block, BlockInfo) = (blockhead,binfo)
    bcache = bcache + cache_entry
    //val newfocused: EnrichedGraphNode = focused.copy( blockinfo = Some(binfo))
    //val entry: (Int, EnrichedGraphNode)  = (blocksym,focused)

    //val tscope = curr + entry
    //val ret = curr -- children.map(k => k._1)
    //(ret,rfullscope)// + entry)
    //(curr + entry ,rfullscope + entry)
    (curr,full)
  }

  /**
   * Called by update_nest - finds all symbols that are transitively bound within the current block
   * Doing this is crucial for code motion - symbols that are not bound within the current block will recognized as "uplinks" in the current
   * scope
   * @param roots the block result symbols from which we backtrack
   * @param backtrack used while recursively calling itself to keep track of where to backtrack (branches in DAG)
   * @param currentmap what we discovered so far
   * @param full the full graph
   * @return the final discovered graph that is bound within the block
   **/

  private def depGraph(roots: Set[Int], backtrack: Set[Int], currentmap: Map[Int,EnrichedGraphNode], full: Map[Int,EnrichedGraphNode]): (Map[Int,EnrichedGraphNode])  = { //private for tailrec
    if (backtrack.isEmpty)
      (currentmap)
    else {
      val track: Int = backtrack.head //backtrack is a stack of nodes we still not to go through
      if (!full.contains(track))
        assert(false, "??")
      val tracknode: EnrichedGraphNode = full(track) //get the head and traverse from there
      val tpredessors = tracknode.predecessors
      val newtrack =  tpredessors filter ( e => !(currentmap.contains(e)) && !roots.contains(e)) //make sure we didnt visit that path already and that we are not at the origin of the subgraph
      //val local_uplink = tracknode.out.filter( x => !full.contains(x))
      //val newuplink = uplink ++ local_uplink
      val newbacktrack = backtrack.tail ++ newtrack //add the alternative paths to the stack
      val entry : (Int,EnrichedGraphNode) = (track,tracknode)
      val newcurrent = currentmap + entry //add the new node of the path to the result
      depGraph(roots,newbacktrack,newcurrent,full) //recurse on this path
    }
  }


  /**
   * This will take the whole DAG and add the reverse dependency information to the nodes
   * (instead of only having the "who do I depend on" - also having "who depends on me"
   * Will be the first thing done during construction of an instance of this class
   * @return An IntMap with TP ID -> EnrichedNode
   */
  protected def enhanceDAG(): Map[Int,EnrichedGraphNode] = {
    //this gives us the dag without the reverse lookup
    val dagmap: Map[Int,EnrichedGraphNode] = id2tp.foldLeft(Map.empty[Int,EnrichedGraphNode]){
      (acc,ele) => acc + DeftoDagEntry(ele._2)
    }

    //creates a hashmap of the reverse edges (one hashmap per origin node)
    val reverse_dag = dagmap map {
      entry => {
        val blockouts: Set[Int] = entry._2.blocks.flatMap( x => x.res.map(y => y.id))
        val outedges = entry._2.predecessors
        val hmap = outedges.foldLeft(scala.collection.immutable.IntMap.empty[Set[Int]]){
              //using IntMap here for the unionWith later
            (acc,ele) => acc + (ele -> Set(entry._1))
          }
        hmap
      }
    }
    //merges those hashmaps
    val merged = reverse_dag.foldLeft(scala.collection.immutable.IntMap.empty[Set[Int]]){
      (acc,ele) => acc.unionWith(ele, (index,a,b: Set[Int]) => a ++ b )
    }

    //create edges for symbols that appear in the graph, but which dont have a Def node
    //examples for such edges would be function parameters or loop indices
    val withpuresyms = merged.foldLeft(dagmap){
      (acc,ele) => {
        if (dagmap.contains(ele._1)) acc
        else {
          //in this case it has no Def
          //in this version of LMS this should never happen - throw an assertion
          assert(false,"it seems we discovered a Symbol without an assoziated TP - this should not happen")
          acc
        }
      }
    }


    //finally fuse with dag to incorporate reverse info
    val full = withpuresyms.foldLeft(Map.empty[Int,EnrichedGraphNode]){
      (acc,ele) => {
        val key = ele._1
        val content = ele._2
        val tp = id2tp(content.irdef)
        val predecessors = content.predecessors
        val successors = merged.get(key).getOrElse(Set.empty[Int])
        val blocks = content.blocks
        val blocksyms = blocks.flatMap(b => b.res.map(r => r.id))         //we remove all symbols from blocks from being bound
        val bound = boundExps(tp.rhs).map( x => x.id).toSet -- blocksyms

        val entry : (Int,EnrichedGraphNode) = (key,EnrichedGraphNode(content.irdef,predecessors,successors,bound,blocks))
        acc + entry
      }
    }
    full
  }


  protected def DeftoDagEntry(defentry: TP[_]): (Int,EnrichedGraphNode) = {
    defentry match {
      case TP(sym: Exp[_], node: Def[_] with Product, tag) => { //pattern match only to get Product - rewrite
        val out = node match {
          case _ => {
            val out = node.productIterator.toSet
            val id = sym.id
            val nsyms = syms(node)
            val precessors = nsyms.map(x => x.id).toSet
            val embedded_blocks = blocks(defentry.rhs).toSet
            val blocksyms = embedded_blocks.flatMap(x => x.res.map(exp => exp.id))
            val precessors_without_blocks = precessors -- blocksyms
            val successors = Set.empty[Int] // we will fill this in a next step
            (id,EnrichedGraphNode(sym.id,precessors_without_blocks,successors,Set.empty[Int],embedded_blocks))
            //(id,EnrichedGraphNode(sym.id,precessors,successors,Set.empty[Int],embedded_blocks))
          }
        }
        out
      }
    }
  }

}


object CodeMotion {
 /** Takes a reified Program as an input. It then follows the dependencies of the the result of the reified program
   * and also stores the reverse edges (for each node - which nodes depend on it).
   * For CodeMotion purpose it also stores for each Block the following information:
   * RF! //update this
   *  Statements that have to be part of the Block (e.g. dependent on Loop iterator)
   *  Statements that are "free" in the block (no dependencies) - this should only happen in the top most block
   *  Uplinks of the current Block (Dependencies of Statements within the block on Statements outside the Bock)
   *
   * @param preifiedIR
   * @author Georg Ofenbeck
   * @return
   */
 def apply(preifiedIR : ReificationPure): CodeMotion{ val reifiedIR: preifiedIR.type} = {
  val cm = new CodeMotion {
   override val reifiedIR: preifiedIR.type = preifiedIR
  }
  cm
 }
}

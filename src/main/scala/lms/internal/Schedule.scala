package scala.lms
package internal

trait Schedule {
 self =>
 //lazy val col = new ScheduleIterable
 val IR: BaseExp
 def choose(tps: Vector[IR.TP[_]]): IR.TP[_] = {
  if (tps.isEmpty){
   assert(false, "this should just not happen")
  }
  tps.head
 }

 class ScheduleIterable(val esc : ExposeScheduleChoice{
  val cminfo : CodeMotion {
   val reifiedIR: ReificationPure {
    val IR: self.IR.type }}}) extends Iterable[self.IR.TP[_]] {

  class ScheduleIterator(val state: esc.MyScheduleChoice) extends Iterator[self.IR.TP[_]] {
   private var typemadness: esc.MyScheduleChoice = state

   //had issues with doing this without a var / therefore the strange name
   private def getTrav(): esc.MyScheduleChoice = typemadness
   private def setTrav(t: esc.MyScheduleChoice): Unit = {
    typemadness = t
   }

   override def hasNext(): Boolean = {
    val t = getTrav()
    !t.scheduleoptions.isEmpty
   }


   override def next(): self.IR.TP[_] = {
    val t = getTrav()
    val id2tp = t.cminfo.reifiedIR.id2tp //short for function

    val tps = t.scheduleoptions.map(p => id2tp(p._1))
    val choice = choose(tps)

    val choice_index = tps.indexOf(choice)
    setTrav(t.scheduleoptions(choice_index)._2())
    choice
    //val tp: esc.cminfo.reifiedIR.IR.TP[_] = id2tp(t.scheduleoptions.head._1)
    //val schedule_options = t.scheduleoptions
    //setTrav(schedule_options.head._2())
    //tp
   }
  }

  def iterator(): Iterator[self.IR.TP[_]] = new ScheduleIterator(esc.getForwardIterator())
  def iterator(block: self.IR.Block): Iterator[self.IR.TP[_]] =
   new ScheduleIterator(esc.getForwardIterator(block))
 }



 def getSchedulewithIterator(esc : ExposeScheduleChoice{
  val cminfo : CodeMotion {
   val reifiedIR: ReificationPure {
    val IR: self.IR.type }}}): ScheduleIterable = new ScheduleIterable(esc)
}


object DefaultSchedule{
 def apply(pIR: BaseExp) = {
  new Schedule {
   override val IR: pIR.type = pIR
  }
 }
}












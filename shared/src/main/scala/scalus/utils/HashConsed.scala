package scalus.utils

import scala.collection.mutable.Map as MutableMap

/**
 * When we read from the HashConsedRead.State, we can have forward references, which will
 * be resolved after the whole structure is readed.
 * Here, HashConsedRef can hold object ro Ref or object of some intermediate representation,
 * which holds other refs.
 *
 * @tparam A
 */
trait HashConsedRef[+A <: AnyRef] {

    /**
     * @return true if we have fully data object here and finValue can be called regradless of the state.
     *         used for optimization.
     */
    def isComplete(hashConsed: HashConsed.State): Boolean

    /**
     * Should be called after the decoding of the whole structures.
     *
     * @return valua of A
     */
    def finValue(hashConsed: HashConsed.State): A

}

object HashConsedRef {
    
    def fromData[A<:AnyRef](a:A): HashConsedRef[A] =
        HashConsed.MutRef.fromData(a)
        
    def fromForward[A<:AnyRef](state: HashConsed.State, ihc: Int, tag: HashConsed.Tag): HashConsedRef[A] =
        state.lookup(ihc, tag) match
            case None => HashConsed.ForwardRef.create(state, ihc, tag)
            case Some(Left(fw)) => 
                         HashConsed.ForwardRef.create(state, ihc, tag)
            case Some(Right(a)) => a.asInstanceOf[HashConsedRef[A]]


    def deferred[A<:AnyRef](complete: HashConsed.State => Boolean,  op: HashConsed.State => A): HashConsedRef[A] =
        new HashConsedRef[A] {
            def isComplete(hashConsed: HashConsed.State) = complete(hashConsed)
            def finValue(hashConsed: HashConsed.State): A = op(hashConsed)
        }

}


/**
 * Objects are hash-consed during restoring from file.
 * Note, that identityHashCode can be different on the different VM-s, so it is not safe to reky on it.
 * value-based hash-code now is not determenistics on recuresive data structures with self-references.
 * So,  we use identityHashCode on VN which store file and restore internal references on other VN.
 */
object HashConsed {

   opaque type Tag = Int

   def tag(value:Int): Tag = value

   class ForwardRefAcceptor(val ihc: Int, val tag:Tag, var setActions: List[HashConsedRef[?]=>Unit]) {
         
       def addAction(action: HashConsedRef[?] => Unit): Unit =
              setActions = action :: setActions
       

   }

    
   class CachedTaggedRef[A<:AnyRef](val tag: Tag, ref: HashConsedRef[A]) extends HashConsedRef[A] {

        private var data: A | Null = null
       
        override def isComplete(hashConsed: State): Boolean =
            data != null ||  ref.isComplete(hashConsed)

        override def finValue(hashConsed:State): A =
            if (data == null) then
                data = ref.finValue(hashConsed)
            data.asInstanceOf[A]
       
   }

   class MutRef[A<:AnyRef](var value: A | Null) extends HashConsedRef[A] {

        override def isComplete(hashConsed: State): Boolean = (value != null)

        override def finValue(hashConsed: State): A =
            if (value == null) then
                throw IllegalStateException("Null reference during reading")
            value.asInstanceOf[A]

        def setValue(a: A): Unit =
            value = a

   }

   object MutRef {
       
        def fromData[A<:AnyRef](a: A): MutRef[A] =
            new MutRef[A](a)

   }

   class ForwardRef[A <: AnyRef](val ihc: Int, val tag:Tag) extends HashConsedRef[A] {

        // not thread-safe, but we are in single-threaded mode
        //  mb later use AtomicReference
        private var ref: HashConsedRef[?] | Null = null
        //var finRef: A | Null = null
        
   
        override def isComplete(hashConsed: State): Boolean =
            ref != null && ref.isComplete(hashConsed)

        override def finValue(hashConsed: State): A =
            if (ref == null) then
                ref = hashConsed.lookup(ihc, tag) match
                    case None =>
                        throw IllegalStateException(s"Forward reference not creaded: $ihc, $tag")
                    case Some(Left(fw)) =>
                        throw IllegalStateException(s"Forward reference not resolved: $ihc, $tag")
                    case Some(Right(a)) =>
                        a
            ref.finValue(hashConsed).asInstanceOf[A]

   }
    

   object ForwardRef {
       def create[A <: AnyRef](state: State, ihc: Int, tag: Tag): HashConsedRef[A] =
           val retval = new ForwardRef[A](ihc, tag)
           val acceptor = new ForwardRefAcceptor(ihc,tag,List((a: HashConsedRef[?]) => retval.ref = a))
           state.putForwardRef(acceptor)
           retval

       /*
       def createTransformed[A, B](state: State, ihc: Int, tag: Tag, fin: (A, State) => B): HashConsedRef[B] =
              var ref: HashConsedRef[A] | Null = null
              val acceptor = new ForwardRefAcceptor(ihc,tag,List(a => ref = a.asInstanceOf[HashConsedRef[A]]))
              state.putForwardRef(acceptor)
              new HashConsedRef[B]:
                  def isComplete = false
                  def finValue(hashConsed: State): B =
                        if ref == null then
                            throw IllegalStateException(s"Forward reference not creaded: $ihc, $tag")
                        fin(ref.finValue(hashConsed), hashConsed)

        */
   }


   /**
    *
    * @param forwardRefAcceptors  - set of forward references, which are not yet resolved from hashConded
    *  @param map  - set of values,
    */
   case class State(
                       forwardRefAcceptors: MutableMap[Int, List[ForwardRefAcceptor]],
                       map: MutableMap[Int, List[CachedTaggedRef[?]]]
                   )  

       

   object State:
       def empty = State(MutableMap.empty, MutableMap.empty)


   def putForwardRefAcceptor(state: State, fw: ForwardRefAcceptor): Boolean =
       val retval = state.forwardRefAcceptors.get(fw.ihc) match
           case None => state.forwardRefAcceptors.put(fw.ihc, List(fw))
                         true
           case Some(l) => l.find(_.tag == fw.tag) match
               case None => state.forwardRefAcceptors.put(fw.ihc, fw :: l)
                            true
               case Some(_) =>
                     false
       //actuall it cna be not forward
       if retval then
              state.map.get(fw.ihc) match
                case None =>
                case Some(l) =>
                     l.find(_.tag == fw.tag) match
                         case None =>
                         case Some(r) =>
                             fw.setActions.foreach(_(r))
       retval


   def setRef[A<:AnyRef](state: State,  ihc: Int, tag: Tag, ra: HashConsedRef[A]): Boolean =
       val retval = state.map.get(ihc) match
           case None => state.map.put(ihc, List(CachedTaggedRef(tag, ra)))
                         true
           case Some(l) => l.find(_.tag == tag) match
               case None => state.map.put(ihc, CachedTaggedRef(tag, ra) :: l)
                            true
               case Some(_) => false
       if retval then
              state.forwardRefAcceptors.get(ihc) match
                case None =>
                case Some(l) =>
                        l.find(_.tag == tag) match
                            case None =>
                            case Some(fw) =>
                                fw.setActions.foreach(_(ra))
       retval


   def lookupValue(s: State, ihc: Int, tag:Tag): Option[HashConsedRef[?]] =
         s.map.get(ihc) match
             case None => None
             case Some(l) => l.find(_.tag == tag) 
                 
   def lookup(s: State, ihc: Int, tag:Tag): Option[Either[ForwardRefAcceptor,HashConsedRef[?]]] =
       lookupValue(s, ihc, tag) match
           case None =>
               s.forwardRefAcceptors.get(ihc) match
                   case None => None
                   case Some(l) =>
                       l.find(_.tag == tag) match
                           case None => None
                           case Some(fw) => Some(Left(fw))
           case Some(r) => Some(Right(r))

   def setForwardRefCallback(s: State, ihc: Int, tag:Tag, setRef: AnyRef => Unit ): Boolean =

       //TODO: introduct accumilatror and make tailRec
       def addSetRef(l:List[ForwardRefAcceptor]): List[ForwardRefAcceptor] =
           l match
             case Nil =>
                  ForwardRefAcceptor(ihc,tag,List(setRef))::Nil
             case h::t =>
                  if h.tag == tag then
                     h.addAction(setRef)
                     h::t
                  else
                     h::addSetRef(t)

       s.forwardRefAcceptors.get(ihc) match
             case None => false
             case Some(l) =>
                    s.forwardRefAcceptors.put(ihc,addSetRef(l))
                    true


   def upsertForwardRefCallback(s: State, ihc: Int, tag:Tag, setRef: AnyRef => Unit ): Boolean =
       val alredyExists = setForwardRefCallback(s, ihc, tag, setRef)
       if (!alredyExists) then   
           putForwardRefAcceptor(s, ForwardRefAcceptor(ihc, tag, List(setRef)))
       else
           false
    
}

extension (s: HashConsed.State)

    def putForwardRef(ihc: Int, tag: HashConsed.Tag, action: AnyRef => Unit): Boolean =
        HashConsed.putForwardRefAcceptor(s, HashConsed.ForwardRefAcceptor(ihc, tag, List(action)))
    
    def putForwardRef(fw: HashConsed.ForwardRefAcceptor): Boolean =
        HashConsed.putForwardRefAcceptor(s, fw)

    def setRef[A<:AnyRef](ihc:Int, tag: HashConsed.Tag, a: HashConsedRef[A]): Boolean =
        HashConsed.setRef(s, ihc, tag, a)
    
    def lookupValue(ihc: Int, tag: HashConsed.Tag): Option[HashConsedRef[?]] =
        HashConsed.lookupValue(s, ihc, tag)
    
    def lookup(ihc: Int, tag: HashConsed.Tag): Option[Either[HashConsed.ForwardRefAcceptor, HashConsedRef[?]]] =
        HashConsed.lookup(s, ihc, tag)
    
    def upsertForwardRefCallback(ihc: Int, tag: HashConsed.Tag, setRef: AnyRef => Unit): Boolean =
        HashConsed.upsertForwardRefCallback(s, ihc, tag, setRef)
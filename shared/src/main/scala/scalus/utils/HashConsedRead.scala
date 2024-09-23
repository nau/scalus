package scalus.utils

import scala.collection.mutable.Map as MutableMap

/**
 * Objects are hash-consed during restoring from file.
 * Note, that identityHashCode can be different on the different VM-s, so it is not safe to reky on it.
 * value-based hash-code now is not determenistics on recuresive data structures with self-references.
 * So,  we use identityHashCode on VN which store file and restore internal references on other VN.
 */
object HashConsedRead {

   opaque type Tag = Int

   def tag(value:Int): Tag = value

   class ForwardRef(val ihc: Int, val tag:Tag, var setActions: List[AnyRef=>Unit]) {
         def addAction(action: AnyRef => Unit): Unit =
              setActions = action :: setActions
   }

   class MutRef[A](var value: A) 
    
   private case class TaggedAnyRef(tag: Tag, ref: AnyRef)

   
    
    /**
     *
      * @param forwardRefs  - set of forward references, which are not yet resolved from hashConded
     *  @param map  - set of values,
     */
   case class State(
                    forwardRefs: MutableMap[Int, List[ForwardRef]],
                    map: MutableMap[Int, List[TaggedAnyRef]]
                   )

   object State:
       def empty = State(MutableMap.empty, MutableMap.empty)


   def putForwardRef(state: State, fw: ForwardRef): Boolean =
       val retval = state.forwardRefs.get(fw.ihc) match
           case None => state.forwardRefs.put(fw.ihc, List(fw))
                         true
           case Some(l) => l.find(_.tag == fw.tag) match
               case None => state.forwardRefs.put(fw.ihc, fw :: l)
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
                             fw.setActions.foreach(_(r.ref))
       retval


   def setRef(state: State,  ihc: Int, tag: Tag, a: AnyRef): Boolean =
       val retval = state.map.get(ihc) match
           case None => state.map.put(ihc, List(TaggedAnyRef(tag, a)))
                         true
           case Some(l) => l.find(_.tag == tag) match
               case None => state.map.put(ihc, TaggedAnyRef(tag, a) :: l)
                            true
               case Some(_) => false
       if retval then
              state.forwardRefs.get(ihc) match
                case None =>
                case Some(l) =>
                        l.find(_.tag == tag) match
                            case None =>
                            case Some(fw) =>
                                fw.setActions.foreach(_(a))
       retval


   def lookupValue(s: State, ihc: Int, tag:Tag): Option[AnyRef] =
         s.map.get(ihc) match
             case None => None
             case Some(l) => l.find(_.tag == tag) match
                 case None => None
                 case Some(r) => Some(r.ref)
                 
   def lookup(s: State, ihc: Int, tag:Tag): Option[Either[ForwardRef,AnyRef]] =
       lookupValue(s, ihc, tag) match
           case None =>
               s.forwardRefs.get(ihc) match
                   case None => None
                   case Some(l) =>
                       l.find(_.tag == tag) match
                           case None => None
                           case Some(fw) => Some(Left(fw))
           case Some(r) => Some(Right(r))

   def setForwardRefCallback(s: State, ihc: Int, tag:Tag, setRef: AnyRef => Unit ): Boolean =

       //TODO: introduct accumilatror and make tailRec
       def addSetRef(l:List[ForwardRef]): List[ForwardRef] =
           l match
              case Nil =>
                  ForwardRef(ihc,tag,List(setRef))::Nil
              case h::t =>
                  if h.tag == tag then
                     h.copy(setActions = setRef :: h.setActions)::t
                  else
                     h::addSetRef(t)

       s.forwardRefs.get(ihc) match
             case None => false
             case Some(l) =>
                    s.forwardRefs.put(ihc,addSetRef(l))
                    true


   def upsertForwardRefCallback(s: State, ihc: Int, tag:Tag, setRef: AnyRef => Unit ): Boolean =
       val alredyExists = setForwardRefCallback(s, ihc, tag, setRef)
       if (!alredyExists) then   
           putForwardRef(s, ForwardRef(ihc, tag, List(setRef)))
       else
           false
    
}

extension (s: HashConsedRead.State)

    def putForwardRef(ihc: Int, tag: HashConsedRead.Tag, action: AnyRef => Unit): Boolean =
        HashConsedRead.putForwardRef(s, HashConsedRead.ForwardRef(ihc, tag, List(action)))
    
    def putForwardRef(fw: HashConsedRead.ForwardRef): Boolean =
        HashConsedRead.putForwardRef(s, fw)

    def setRef(ihc:Int, tag: HashConsedRead.Tag, a: AnyRef): Boolean =
        HashConsedRead.setRef(s, ihc, tag, a)
    
    def lookupValue(ihc: Int, tag: HashConsedRead.Tag): Option[AnyRef] =
        HashConsedRead.lookupValue(s, ihc, tag)
    
    def lookup(ihc: Int, tag: HashConsedRead.Tag): Option[Either[HashConsedRead.ForwardRef, AnyRef]] =
        HashConsedRead.lookup(s, ihc, tag)
    
    def upsertForwardRefCallback(ihc: Int, tag: HashConsedRead.Tag, setRef: AnyRef => Unit): Boolean =
        HashConsedRead.upsertForwardRefCallback(s, ihc, tag, setRef)
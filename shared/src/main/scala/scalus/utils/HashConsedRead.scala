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

   case class ForwardRef(ihc: Int, tag:Tag, setActions: List[AnyRef=>Unit])

   case class TaggedRef(tag: Tag, ref: AnyRef)

    /**
     *
      * @param forwardRefs  - set of forward references, which are not yet resolved from hashConded
     *  @param map  - set of values,
     */
   case class State(
                    forwardRefs: MutableMap[Int, List[ForwardRef]],
                    map: MutableMap[Int, List[TaggedRef]]
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
           case None => state.map.put(ihc, List(TaggedRef(tag, a)))
                         true
           case Some(l) => l.find(_.tag == tag) match
               case None => state.map.put(ihc, TaggedRef(tag, a) :: l)
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

   def lookupForwardRef(s: State, ihc: Int, tag:Tag, setRef: AnyRef => Unit ): Boolean =

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


}

extension (s: HashConsedRead.State)

    def putForwardRef(fw: HashConsedRead.ForwardRef): Boolean =
        HashConsedRead.putForwardRef(s, fw)

    def setRef(ihc:Int, tag: HashConsedRead.Tag, a: AnyRef): Boolean =
        HashConsedRead.setRef(s, ihc, tag, a)

    def lookupValue(ihc: Int, tag: HashConsedRead.Tag): Option[AnyRef] =
        HashConsedRead.lookupValue(s, ihc, tag)

    def lookupForwardRef(ihc: Int, tag: HashConsedRead.Tag, setRef: AnyRef => Unit): Boolean =
        HashConsedRead.lookupForwardRef(s, ihc, tag, setRef)
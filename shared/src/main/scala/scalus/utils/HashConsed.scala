package scalus.utils

import scala.collection.mutable.Map as MutableMap

object HashConsed {

   opaque type Map = MutableMap[Int,List[AnyRef]]

   def empty: Map = MutableMap.empty[Int,List[AnyRef]]

   case class Ref(hc: Int, i: Int)

   case class ForwardRef(hc: Int)

   def putRef(m: Map, a: AnyRef): Int =
       val hc = a match
           case ForwardRef(hc) => hc
           case _ => a.hashCode()
       m.get(hc) match
           case None =>
               m.put(hc, List(a))
               0
           case Some(l) =>
               l.indexWhere(_.equals(a)) match
                   case -1 =>
                       m.put(hc, a :: l)
                       0
                   case i => i

   def getRef(m: Map, ref: Ref): AnyRef =
       m.get(ref.hc) match
           case None => throw new Exception(s"HashConsed.getRef: $ref")
           case Some(l) => l(ref.i)

   def getOptRef(m: Map, ref: Ref): Option[AnyRef] =
       m.get(ref.hc) match
           case None => None
           case Some(l) => l.lift(ref.i)

   def lookup(m: Map, a: AnyRef): Option[Ref] =
       val hc = a match
           case ForwardRef(hc) => hc
           case _ => a.hashCode()
       m.get(hc) match
           case None => None
           case Some(l) => l.indexWhere(_.equals(a)) match
               case -1 => None
               case i => Some(Ref(hc, i))

}

extension (m: HashConsed.Map)

    def putRef(a: AnyRef): Int =
        HashConsed.putRef(m, a)

    def getOptRef(ref: HashConsed.Ref): Option[AnyRef] =
        HashConsed.getOptRef(m, ref)

    def getRef(ref: HashConsed.Ref): AnyRef =
        HashConsed.getRef(m, ref)

    def lookup(a: AnyRef): Option[HashConsed.Ref] =
        HashConsed.lookup(m, a)
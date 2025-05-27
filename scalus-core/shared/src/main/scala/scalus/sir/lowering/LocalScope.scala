package scalus.sir.lowering

import scalus.sir.SIRPosition

type LocalScope = LocalScope.Carrier


case class LocalScopeRecord(
    name: String,
    value: LoweredValue,
    otherRepresentations: Map[LoweredValueRepresentation, LoweredValue]
)

object LocalScope {

    opaque type Carrier = Map[String, LocalScopeRecord]

    def empty: Carrier = Map.empty

    def add(
               scope: Carrier,
               name: String,
               value: LoweredValue,
               pos: SIRPosition
    ): Carrier = {
        scope.get(name) match {
            case None =>
                scope.updated(name, LocalScopeRecord(name, value, Map.empty))
            case Some(record)  =>
                if value.representation == record.value.representation then
                    throw LoweringException(
                        s"Value with name $name already exists in the scope with the same representation", pos
                    )
                else record.otherRepresentations.get(value.representation) match {
                    case Some(_) =>
                        throw LoweringException(
                            s"Value with name $name already exists in the scope with the same representation", pos
                        )
                    case None =>
                        val newOtherRepresentations = record.otherRepresentations.updated(
                            value.representation,
                            value
                        )
                        scope.updated(
                            name,
                            record.copy(otherRepresentations = newOtherRepresentations)
                        )
                }
        }
    }

    def get(scope: Carrier, name: String): Option[LocalScopeRecord] = {
        scope.get(name)
    }

}

extension (c: LocalScope)

    def put(
        name: String,
        value: LoweredValue,
        pos: SIRPosition
    ): LocalScope = LocalScope.add(c, name, value, pos)

    def get(name: String): Option[LocalScopeRecord] = LocalScope.get(c, name)



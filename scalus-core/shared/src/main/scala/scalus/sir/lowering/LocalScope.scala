package scalus.sir.lowering

import scalus.sir.SIRPosition

case class LocalScope(
    byId: Map[String, VariableLoweredValue],
    byName: Map[String, VariableLoweredValue]
) {

    def getById(id: String): Option[VariableLoweredValue] = byId.get(id)

    def getByName(name: String): Option[VariableLoweredValue] = byName.get(name)

    def getOrCreateById(
        id: String,
        representation: LoweredValueRepresentation,
        pos: SIRPosition
    )(using LoweringContext): Option[IdentifiableLoweredValue] = {
        byId.get(id) match {
            case Some(v) =>
                if v.representation == representation then Some(v)
                else
                    v.otherRepresentations.get(representation) match
                        case Some(other) => Some(other)
                        case None =>
                            val newVarRhs = v.toRepresentation(representation, pos)
                            val newVar = new DependendVariableLoweredValue(
                              id = id,
                              name = v.name,
                              sir = v.sir,
                              representation = representation,
                              rhs = newVarRhs
                            )
                            v.otherRepresentations.update(representation, newVar)
                            Some(newVar)
            case None => None
        }
    }

    def get(
        id: String,
        representation: LoweredValueRepresentation
    ): Option[IdentifiableLoweredValue] = {
        byId.get(id).flatMap { v =>
            if v.representation == representation then Some(v)
            else v.otherRepresentations.get(representation)
        }
    }

    def add(value: VariableLoweredValue): LocalScope = {
        if byId.contains(value.id) then
            throw new IllegalArgumentException(s"Variable ${value.id} already exists in the scope")
        else LocalScope(byId + (value.id -> value), byName + (value.name -> value))
    }

    def addAll(values: Iterable[VariableLoweredValue]): LocalScope = {
        values.foldLeft(this) { (scope, value) =>
            scope.add(value)
        }
    }

}

object LocalScope {

    def empty: LocalScope = LocalScope(Map.empty, Map.empty)

}

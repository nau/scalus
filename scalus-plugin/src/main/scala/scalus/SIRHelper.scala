package scalus

import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.ast.tpd.MemberDef
import dotty.tools.dotc.util.{NoSourcePosition, SourcePosition, SrcPos}
import scalus.sir.SIRPosition
import scalus.sir.AnnotationsDecl
import dotty.tools.dotc.core.Contexts.Context

extension (singleton: SIRPosition.type)
    def fromSrcPos(pos: SrcPos)(using Context): SIRPosition =
        if pos.span.exists then
            SIRPosition(
              pos.startPos.source.path,
              pos.startPos.startLine,
              pos.startPos.startColumn,
              pos.endPos.endLine,
              pos.endPos.endColumn
            )
        else SIRPosition.empty

    def fromSourcePosition(pos: SourcePosition)(using Context): SIRPosition =
        if pos.span.exists then
            SIRPosition(
              pos.source.path,
              pos.startLine,
              pos.startColumn,
              pos.endLine,
              pos.endColumn
            )
        else SIRPosition.empty

extension (singleton: AnnotationsDecl.type)
    def fromSrcPos(pos: SrcPos)(using Context): AnnotationsDecl =
        AnnotationsDecl(
          SIRPosition.fromSrcPos(pos)
        )

    def fromSourcePosition(pos: SourcePosition)(using Context): AnnotationsDecl =
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(pos)
        )

    def fromSym(sym: Symbol)(using Context): AnnotationsDecl =
        val optComment = sym.defTree match
            case memberDef: MemberDef => memberDef.rawComment.map(_.raw)
            case _                    => None
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(sym.sourcePos),
          optComment
        )

    def fromSymIn(sym: Symbol, inPos: SourcePosition)(using Context): AnnotationsDecl =
        val optComment = sym.defTree match
            case memberDef: MemberDef => memberDef.rawComment.map(_.raw)
            case _                    => None
        val pos =
            if sym.sourcePos == NoSourcePosition then inPos
            else if inPos == NoSourcePosition then sym.sourcePos
            else inPos
        AnnotationsDecl(
          SIRPosition.fromSourcePosition(pos),
          optComment
        )

extension (pos: SourcePosition)
    infix def union(other: SourcePosition): SourcePosition =
        if pos == NoSourcePosition then other
        else if other == NoSourcePosition then pos
        else SourcePosition(pos.source, pos.span.union(other.span), NoSourcePosition)


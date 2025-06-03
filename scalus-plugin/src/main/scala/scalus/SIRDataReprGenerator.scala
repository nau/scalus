package scalus

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.report
import dotty.tools.dotc.util.{NoSourcePosition, SrcPos}
import scalus.sir.*
import scalus.sir.Recursivity.NonRec
import scalus.sir.SIR.Builtin
import scalus.uplc.Constant

class SIRDataReprGenerator(using ctx: Context) {

    case class Env(
        typeVars: Map[SIRType.TypeVar, SIRType] = Map.empty,
        zCominatorNeeded: Boolean = false,
        typeArgContextParams: Map[SIRType.TypeVar, SIR.Var] = Map.empty,
        srcPos: SrcPos = NoSourcePosition
    )

    object Env {
        def empty: Env = Env()
    }

    val sirTpListData = SIRType.List(SIRType.Data)

    def generate(fullName: FullName, tp: SIRType, srcPos: SrcPos): SIR = {
        println(s"SIRDataReprGenerator::generate, fullName=${fullName.name}, tp=${tp.show}")
        if fullName.name.endsWith("derived$FromData") then
            tp match
                case SIRType.Fun(in, out) =>
                    generateFromDataFun(out, Env.empty.copy(srcPos = srcPos))
                case _ =>
                    val message = s"Invalid type for derived FromData for ${fullName}: ${tp.show}"
                    report.error(message, srcPos)
                    SIR.Error(message, AnnotationsDecl.fromSrcPos(srcPos))
        else if fullName.name.endsWith("derived$ToData") then
            tp match
                case SIRType.Fun(in, out) =>
                    generateToDataFun(tp, Env.empty.copy(srcPos = srcPos))
                case _ =>
                    val message = s"Invalid type for derived ToData for ${fullName}: ${tp.show}"
                    report.error(message, srcPos)
                    SIR.Error(message, AnnotationsDecl.fromSrcPos(srcPos))
        else
            val message = s"Only FromData/ToData derivatons is supported, we have ${fullName}"
            SIR.Error(message, AnnotationsDecl.fromSrcPos(srcPos))
    }

    def generateToDataFun(tp: SIRType, env: Env): SIR = {
        val inputVar = SIR.Var("input", tp, AnnotationsDecl.empty)
        tp match
            case SIRType.TypeLambda(params, body) =>
                val newTypeArgsContextParams = params.map { param =>
                    param -> SIR.Var(
                      param.name,
                      SIRType.Fun(SIRType.Data, param),
                      AnnotationsDecl.empty
                    )
                }.toMap
                val newTypeVars = params.map { _ -> SIRType.FreeUnificator }.toMap
                val nEnv = env.copy(
                  typeVars = env.typeVars ++ newTypeVars,
                  typeArgContextParams = env.typeArgContextParams ++ newTypeArgsContextParams
                )
                val internalDerives: SIR = generateToDataRhs(tp, nEnv, inputVar)
                val generatedWithParams = params.foldRight(internalDerives) { (param, acc) =>
                    SIR.LamAbs(newTypeArgsContextParams(param), acc, AnnotationsDecl.empty)
                }
                val retval = SIR.LamAbs(inputVar, generatedWithParams, AnnotationsDecl.empty)
                retval
            case SIRType.TypeProxy(ref) =>
                generateToDataFun(ref, env)
            case _ =>
                SIR.LamAbs(inputVar, generateToDataRhs(tp, env, inputVar), AnnotationsDecl.empty)
    }

    def generateFromDataFun(tp: SIRType, env: Env): SIR = {
        val dataVar = SIR.Var("data", SIRType.Data, AnnotationsDecl.empty)
        tp match {
            case SIRType.TypeLambda(params, body) =>
                // we expect that type-lambdas define a paramaters for function which mean fromData for type arguments.
                val newTypeArgsContextParams = params.map { param =>
                    val sirVar =
                        SIR.Var(param.name, SIRType.Fun(param, SIRType.Data), AnnotationsDecl.empty)
                    param -> sirVar
                }.toMap
                val newTypeVars = params.map { _ -> SIRType.FreeUnificator }.toMap
                val nEnv = env.copy(
                  typeVars = env.typeVars ++ newTypeVars,
                  typeArgContextParams = env.typeArgContextParams ++ newTypeArgsContextParams
                )
                val internalDerives: SIR = generateFromDataRhs(body, nEnv, dataVar)
                val generatedWithParams = params.foldRight(internalDerives) { (param, acc) =>
                    SIR.LamAbs(newTypeArgsContextParams(param), acc, AnnotationsDecl.empty)
                }
                val retval = SIR.LamAbs(dataVar, generatedWithParams, AnnotationsDecl.empty)
                retval
            case SIRType.TypeProxy(ref) =>
                generateFromDataFun(ref, env)
            case _ =>
                SIR.LamAbs(
                  dataVar,
                  generateFromDataRhs(tp, env, dataVar),
                  AnnotationsDecl.empty
                )

        }
    }

    def resolveFromDataRhs(tp: SIRType, env: Env, dataExpr: SIR): SIR = {
        // TODO: check recursion
        tp match {
            case SIRType.TypeProxy(ref) =>
                resolveFromDataRhs(ref, env, dataExpr)
            case SIRType.TypeLambda(params, body) =>
                ???
            case SIRType.SumCaseClass(decl, typeArgs) =>
                // Hmm, we can resolve, if we have compiler.
                //  But if not ... assume derivedd.
                val args = typeArgs.map(t => resolveFromDataRhs(t, env, dataExpr))

                val fromDataVarTp: SIRType = {
                    decl.typeParams.foldRight(SIRType.Fun(SIRType.Data, tp): SIRType) { (e, s) =>
                        val fromE = SIRType.Fun(SIRType.Data, e)
                        SIRType.Fun(fromE, s)
                    }
                }

                val fromDataVar: SIR = SIR.ExternalVar(
                  decl.name + '$',
                  decl.name + "$.derived$FromData",
                  fromDataVarTp,
                  AnnotationsDecl.empty
                )

                val (sir, rtp) = args.foldLeft((fromDataVar, fromDataVarTp)) {
                    case ((sSIR, sType), e) =>
                        val nextType = sType match
                            case SIRType.Fun(ex, ey) => ey
                            case _ =>
                                val message = s"Expected function type, but we have ${sType.show}"
                                report.error(message, env.srcPos)
                                sType
                        (SIR.Apply(sSIR, e, nextType, AnnotationsDecl.empty), nextType)
                }
                sir

            case SIRType.CaseClass(constrDecl, typeArgs, parent) =>
                ???

            case SIRType.Integer =>
                SIR.Apply(SIRBuiltins.iData, dataExpr, SIRType.Integer, AnnotationsDecl.empty)
            case SIRType.ByteString =>
                SIR.Apply(SIRBuiltins.bData, dataExpr, SIRType.ByteString, AnnotationsDecl.empty)
            case SIRType.String =>
                SIR.Apply(
                  SIRBuiltins.encodeUtf8,
                  SIR.Apply(SIRBuiltins.bData, dataExpr, SIRType.ByteString, AnnotationsDecl.empty),
                  SIRType.String,
                  AnnotationsDecl.empty
                )
            case SIRType.Unit =>
                SIR.Const(
                  Constant.Unit,
                  SIRType.Unit,
                  AnnotationsDecl.empty
                )
            case SIRType.Data =>
                dataExpr

        }
    }

    def generateFromDataRhs(tp: SIRType, env: Env, input: SIR.Var): SIR = {
        tp match {
            case SIRType.SumCaseClass(decls, typeArgs) =>
                generateFromDataRhsSum(tp, decls, typeArgs, env, input)
            case cc @ SIRType.CaseClass(constrDecl, targs, parent) =>
                generateFromDataRhsCaseClass(cc, env, input)
            case SIRType.TypeProxy(ref) =>
                generateFromDataRhs(ref, env, input)
            case SIRType.TypeLambda(params, body) =>
                report.error("Carried type arguments are not supported yet", env.srcPos)
                SIR.Error("Carried type arguments are not supported yet", AnnotationsDecl.empty)
            case v: SIRType.TypeVar =>
                env.typeArgContextParams.get(v) match
                    case Some(sirVar) =>
                        SIR.Apply(sirVar, input, SIRType.Data, AnnotationsDecl.empty)
                    case None =>
                        val message = s"Cannot find type argument context parameter for ${v.show}"
                        report.error(message, env.srcPos)
                        SIR.Error(message, AnnotationsDecl.fromSrcPos(env.srcPos))
            case SIRType.Data => input
            case SIRType.Integer =>
                SIR.Apply(SIRBuiltins.unIData, input, SIRType.Integer, AnnotationsDecl.empty)
            case SIRType.ByteString =>
                SIR.Apply(SIRBuiltins.unBData, input, SIRType.ByteString, AnnotationsDecl.empty)
            case SIRType.String =>
                SIR.Apply(
                  SIRBuiltins.decodeUtf8,
                  SIR.Apply(
                    SIRBuiltins.unBData,
                    input,
                    SIRType.ByteString,
                    AnnotationsDecl.empty
                  ),
                  SIRType.String,
                  AnnotationsDecl.empty
                )
            case SIRType.Unit => SIR.Const(Constant.Unit, SIRType.Unit, AnnotationsDecl.empty)
            case _ =>
                val message = s"Cannot generate FromData for ${tp.show}"
                report.error(message, env.srcPos)
                SIR.Error(message, AnnotationsDecl.fromSrcPos(env.srcPos))
        }
    }

    def generateFromDataRhsSum(
        sumTp: SIRType,
        decl: DataDecl,
        typeArgs: List[SIRType],
        env: Env,
        input: SIR.Var
    ): SIR = {
        val pairVar = SIR.Var(
          "pair",
          SIRType.Pair(SIRType.Integer, SIRType.List(SIRType.Data)),
          AnnotationsDecl.empty
        )
        val pairRhs = SIR.Apply(
          SIRBuiltins.unConstrData,
          input,
          SIRType.Pair(SIRType.Integer, sirTpListData),
          AnnotationsDecl.empty
        )
        val frsVar = SIR.Var("frs", SIRType.Integer, AnnotationsDecl.empty)
        val sndVar = SIR.Var("snd", SIRType.List(SIRType.Data), AnnotationsDecl.empty)

        val ifBranches = decl.constructors.zipWithIndex.map { (constr, constrIndex) =>
            val check = SIR.Apply(
              SIR.Apply(
                SIRBuiltins.equalsInteger,
                frsVar,
                SIRType.Fun(SIRType.Integer, SIRType.Boolean),
                AnnotationsDecl.empty
              ),
              SIR.Const(Constant.Integer(constrIndex), SIRType.Integer, AnnotationsDecl.empty),
              SIRType.Boolean,
              AnnotationsDecl.empty
            )
            val constrTypeArgs =
                if constr.typeParams.isEmpty then Nil
                else {
                    SIRUnify.unifyList(constr.parentTypeArgs, typeArgs, SIRUnify.Env.empty) match
                        case SIRUnify.UnificationSuccess(env, unificator) =>
                            constr.typeParams.map { t =>
                                env.filledTypes.getOrElse(t, SIRType.FreeUnificator)
                            }
                        case SIRUnify.UnificationFailure(path, left, s) =>
                            report.error(
                              s"Incmosistent type parameters of ${decl.name} for case ${constr.name}: $path, $left, $s",
                              env.srcPos
                            )
                            Nil
                }
            val nTypeVars =
                constr.typeParams.zip(constrTypeArgs).map { case (param, t) => param -> t }.toMap
            val nEnv = env.copy(typeVars = env.typeVars ++ nTypeVars)
            val constrSir = generateFromDataConstr(constr, constrTypeArgs, decl, nEnv, sumTp, input)
            (check, constrSir)
        }
        val lastElse: AnnotatedSIR =
            SIR.Error(s"bad data encoding for ${decl.name}", AnnotationsDecl.empty)
        val ifChecks = ifBranches.foldRight(lastElse) { case ((check, branch), s) =>
            SIR.Apply(
              SIR.Apply(
                SIR.Apply(
                  SIRBuiltins.ifThenElse,
                  check,
                  SIRType.Fun(sumTp, SIRType.Fun(sumTp, sumTp)),
                  AnnotationsDecl.empty
                ),
                branch,
                SIRType.Fun(sumTp, sumTp),
                AnnotationsDecl.empty
              ),
              s,
              sumTp,
              AnnotationsDecl.empty
            )
        }
        val retval =
            SIR.Let(
              Recursivity.NonRec,
              List(Binding(pairVar.name, pairRhs)),
              SIR.Let(
                Recursivity.NonRec,
                List(
                  Binding(
                    frsVar.name,
                    SIR.Apply(SIRBuiltins.fstPair, pairVar, SIRType.Data, AnnotationsDecl.empty)
                  ),
                  Binding(
                    sndVar.name,
                    SIR.Apply(SIRBuiltins.sndPair, pairVar, sirTpListData, AnnotationsDecl.empty)
                  )
                ),
                ifChecks,
                AnnotationsDecl.empty
              ),
              AnnotationsDecl.empty
            )
        retval
    }

    def generateFromDataRhsCaseClass(
        caseType: SIRType.CaseClass,
        env: Env,
        input: SIR.Var
    ): SIR = {
        if caseType.parent.isDefined then {
            val message =
                s"FromData for ${caseType.constrDecl.name} should be declared for its parent ${caseType.parent.get.show}"
            report.error(message, env.srcPos)
            SIR.Error(message, AnnotationsDecl.fromSrcPos(env.srcPos))
        }
        retrieveParentDataDecl(caseType.constrDecl, caseType.typeArgs, caseType.parent) match {
            case Left(message) =>
                report.error(message, env.srcPos)
                SIR.Error(message, AnnotationsDecl.fromSrcPos(env.srcPos))
            case Right(parentDecl) =>
                generateFromDataConstr(
                  caseType.constrDecl,
                  caseType.typeArgs,
                  parentDecl,
                  env,
                  caseType,
                  input
                )
        }
    }

    def generateFromDataConstr(
        constrDecl: ConstrDecl,
        constrTypeArgs: List[SIRType],
        dataDecl: DataDecl,
        env: Env,
        resType: SIRType,
        dataVar: SIR.Var
    ): SIR = {
        val typeVars = constrDecl.typeParams.zip(constrTypeArgs).toMap
        val nEnv = env.copy(typeVars = env.typeVars ++ typeVars)
        val constrParams =
            constrDecl.params.map(tv => SIR.Var(tv.name, tv.tp, AnnotationsDecl.empty))
        val constrRhs: AnnotatedSIR = SIR.Constr(
          constrDecl.name,
          dataDecl,
          constrParams,
          SIRType.typeApply(dataDecl.constrType(constrDecl.name), constrTypeArgs),
          AnnotationsDecl.empty
        )
        val constrParamsIndexed = constrParams.toIndexedSeq
        val nParams = constrParamsIndexed.size
        val (constrSir, _) = constrParamsIndexed.foldRight((constrRhs, nParams - 1)) {
            case (e, (sir, i)) =>
                val prevTailName = if i == 0 then dataVar.name else s"sndL${i - 1}"
                val prevTail =
                    if i == 0 then dataVar
                    else SIR.Var(prevTailName, sirTpListData, AnnotationsDecl.empty)
                val paramVar = constrParamsIndexed(i)
                // val paramVarDataName = s"${paramVar.name}Data"
                val paramVarDataValue =
                    SIR.Apply(SIRBuiltins.headList, prevTail, paramVar.tp, AnnotationsDecl.empty)
                val paramVarValue =
                    resolveFromDataRhs(paramVar.tp, env, paramVarDataValue)
                val paramVarBinding = Binding(paramVar.name, paramVarValue)
                val newSir = {
                    if i < nParams - 1 then
                        val currentTailName = s"sndL$i"
                        val currentTailValue =
                            SIR.Apply(
                              SIRBuiltins.tailList,
                              prevTail,
                              sirTpListData,
                              AnnotationsDecl.empty
                            )
                        val currentTailBinding = Binding(currentTailName, currentTailValue)
                        SIR.Let(
                          NonRec,
                          List(paramVarBinding, currentTailBinding),
                          sir,
                          AnnotationsDecl.empty
                        )
                    else SIR.Let(NonRec, List(paramVarBinding), sir, AnnotationsDecl.empty)
                }
                (newSir, i - 1)
        }
        constrSir
    }

    def generateToDataRhs(sirType: SIRType, env: Env, input: SIR.Var): SIR = {
        sirType match {
            case SIRType.SumCaseClass(decl, typeArgs) =>
                // generateToDataRhsSum(sirType, decl, typeArgs, env, input)
                ???
        }
    }

    def retrieveParentDataDecl(
        constrDecl: ConstrDecl,
        typeArgs: List[SIRType],
        parent: Option[SIRType]
    ): Either[String, DataDecl] = {
        parent
            .map { pt =>
                pt match {
                    case SIRType.SumCaseClass(decl, pTypeArgs) => Right(decl)
                    case SIRType.TypeProxy(ref) =>
                        retrieveParentDataDecl(constrDecl, typeArgs, Some(ref))
                    case SIRType.TypeLambda(tparams, body) =>
                        retrieveParentDataDecl(constrDecl, typeArgs, Some(body))
                    case _ =>
                        Left(
                          s"parent type for ${constrDecl.name} is ${pt.show} which is impossible"
                        )
                }
            }
            .getOrElse(
              Right(
                DataDecl(
                  constrDecl.name,
                  List(constrDecl),
                  constrDecl.typeParams,
                  AnnotationsDecl.empty
                )
              )
            )
    }

}

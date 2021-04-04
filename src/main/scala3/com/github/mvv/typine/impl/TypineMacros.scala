package com.github.mvv.typine.impl

import com.github.mvv.typine.!:=

import scala.annotation.tailrec
import scala.quoted.*

object TypineMacros:
  enum ComparedTypes:
    case Same
    case Different
    case Unknown

  @tailrec
  private def isStableQualifier(qctx: Quotes)(tpe: qctx.reflect.TypeRepr): Boolean =
    import qctx.reflect._
    tpe match
      case tr: TermRef =>
        val flags = tr.termSymbol.flags
        if flags.is(Flags.Module) || flags.is(Flags.Package) then
          isStableQualifier(qctx)(tr.qualifier)
        else
          false
      case ThisType(tr: TypeRef) =>
        val flags = tr.typeSymbol.flags
        if flags.is(Flags.Package) then
          isStableQualifier(qctx)(tr.qualifier)
        else
          false
      case NoPrefix() =>
        true
      case _ =>
        false

  private def isStableTypeRef(qctx: Quotes)(tr: qctx.reflect.TypeRef): Boolean =
    import qctx.reflect._
    val flags = tr.typeSymbol.flags
    !flags.is(Flags.Deferred) && !flags.is(Flags.Param) && isStableQualifier(qctx)(tr.qualifier)

  def compareTypeReprs(qctx: Quotes)(t1: qctx.reflect.TypeRepr, t2: qctx.reflect.TypeRepr): ComparedTypes =
    import qctx.reflect._
    def recur(t1: TypeRepr, t2: TypeRepr): ComparedTypes =
      if t1 =:= t2 then
        ComparedTypes.Same
      else
        (t1.dealias.simplified, t2.dealias.simplified) match
          case (tr1: TypeRef, tr2: TypeRef) =>
            if isStableTypeRef(qctx)(tr1) && isStableTypeRef(qctx)(tr2) then
              ComparedTypes.Different
            else
              ComparedTypes.Unknown
          case (tr1: TypeRef, AppliedType(tcon2, _)) =>
            tcon2.dealias.simplified match
              case tr2: TypeRef =>
                if isStableTypeRef(qctx)(tr1) && isStableTypeRef(qctx)(tr2) then
                  ComparedTypes.Different
                else
                  ComparedTypes.Unknown
              case _ =>
                ComparedTypes.Unknown
          case (AppliedType(tcon1, _), tr2: TypeRef) =>
            tcon1.dealias.simplified match
              case tr1: TypeRef =>
                if isStableTypeRef(qctx)(tr1) && isStableTypeRef(qctx)(tr2) then
                  ComparedTypes.Different
                else
                  ComparedTypes.Unknown
              case _ =>
                ComparedTypes.Unknown
          case (AppliedType(tcon1, targs1), AppliedType(tcon2, targs2)) =>
            recur(tcon1, tcon2) match
              case ComparedTypes.Different =>
                ComparedTypes.Different
              case ComparedTypes.Same =>
                targs1.iterator.zip(targs2.iterator).foreach {
                  case (targ1, targ2) =>
                    Implicits.search(TypeRepr.of(using Type.of[!:=](using qctx)).appliedTo(List(targ1, targ2))) match
                      case _: ImplicitSearchSuccess =>
                        return ComparedTypes.Different
                      case _: ImplicitSearchFailure =>
                }
                ComparedTypes.Unknown
              case ComparedTypes.Unknown =>
                ComparedTypes.Unknown
          case _ =>
            ComparedTypes.Unknown
    recur(t1, t2)

  def deriveUnequal[A: Type, B: Type](using qctx: Quotes): Expr[A !:= B] =
    import qctx.reflect._
    compareTypeReprs(qctx)(TypeRepr.of[A], TypeRepr.of[B]) match
      case ComparedTypes.Different =>
        '{(!:=).unsafeMake[A, B]}
      case ComparedTypes.Same =>
        report.throwError(s"types ${Type.show[A]} and ${Type.show[B]} are the same")
      case ComparedTypes.Unknown =>
        report.throwError(s"could not prove that types ${Type.show[A]} and ${Type.show[B]} are different")

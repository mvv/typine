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
  private def isStableQualifier(using qctx: Quotes)(tpe: qctx.reflect.TypeRepr): Boolean =
    import qctx.reflect._
    tpe match
      case tr: TermRef =>
        val flags = tr.termSymbol.flags
        if flags.is(Flags.Module) || flags.is(Flags.Package) then
          isStableQualifier(tr.qualifier)
        else
          false
      case ThisType(tr: TypeRef) =>
        val flags = tr.typeSymbol.flags
        if flags.is(Flags.Package) then
          isStableQualifier(tr.qualifier)
        else
          false
      case NoPrefix() =>
        true
      case _ =>
        false

  private def isStableTypeRef(using qctx: Quotes)(tr: qctx.reflect.TypeRef): Boolean =
    import qctx.reflect._
    val flags = tr.typeSymbol.flags
    !flags.is(Flags.Deferred) && !flags.is(Flags.Param) && isStableQualifier(tr.qualifier)

  def searchUnequal(using qctx: Quotes)(
        t1: qctx.reflect.TypeRepr, t2: qctx.reflect.TypeRepr): qctx.reflect.ImplicitSearchResult =
    import qctx.reflect._
    Implicits.search(TypeRepr.of[!:=].appliedTo(List(t1, t2)))

  def compareTypeReprs(using qctx: Quotes)(t1: qctx.reflect.TypeRepr, t2: qctx.reflect.TypeRepr): ComparedTypes =
    import qctx.reflect._
    if t1 =:= t2 then
      ComparedTypes.Same
    else
      (t1.dealias.simplified, t2.dealias.simplified) match
        case (tr1: TypeRef, tr2: TypeRef) =>
          if isStableTypeRef(tr1) && isStableTypeRef(tr2) then
            ComparedTypes.Different
          else
            ComparedTypes.Unknown
        case (tr1: TypeRef, AppliedType(tcon2: TypeRef, _)) =>
          if isStableTypeRef(tr1) && isStableTypeRef(tcon2) then
            ComparedTypes.Different
          else
            ComparedTypes.Unknown
        case (AppliedType(tcon1: TypeRef, _), tr2: TypeRef) =>
          if isStableTypeRef(tcon1) && isStableTypeRef(tr2) then
            ComparedTypes.Different
          else
            ComparedTypes.Unknown
        case (AppliedType(tcon1: TypeRef, targs1), AppliedType(tcon2: TypeRef, targs2)) =>
          if isStableTypeRef(tcon1) && isStableTypeRef(tcon2) then
            if tcon1 =:= tcon2 then
              targs1.iterator.zip(targs2.iterator).foreach {
                case (targ1, targ2) =>
                  searchUnequal(targ1, targ2) match
                    case _: ImplicitSearchSuccess =>
                      return ComparedTypes.Different
                    case _: ImplicitSearchFailure =>
              }
              ComparedTypes.Unknown
            else
              ComparedTypes.Different
          else
            ComparedTypes.Unknown
        case _ =>
          ComparedTypes.Unknown

  def deriveUnequal[A: Type, B: Type](using qctx: Quotes): Expr[A !:= B] =
    import qctx.reflect._
    compareTypeReprs(TypeRepr.of[A], TypeRepr.of[B]) match
      case ComparedTypes.Different =>
        '{(!:=).unsafeMake[A, B]}
      case ComparedTypes.Same =>
        report.throwError(s"types ${Type.show[A]} and ${Type.show[B]} are the same")
      case ComparedTypes.Unknown =>
        report.throwError(s"could not prove that types ${Type.show[A]} and ${Type.show[B]} are different")

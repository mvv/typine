package com.github.mvv.typine.impl

import com.github.mvv.typine.!:=

import scala.annotation.tailrec
import scala.quoted.*

object TypineMacros:
  private def isStableQualifier(using qctx: Quotes)(tpe: qctx.reflect.TypeRepr): Boolean =
    import qctx.reflect.*
    @tailrec
    def isStable(inner: TypeRepr): Boolean =
      inner match
        case tr: TermRef =>
          val sym = tr.termSymbol
          val flags = sym.flags
          if flags.is(Flags.Module) || flags.is(Flags.Package) then isStable(tr.qualifier)
          else sym.isValDef && isStable(tr.widenTermRefByName)
        case tr: TypeRef =>
          val flags = tr.typeSymbol.flags
          (flags.is(Flags.Module) || flags.is(Flags.Package)) && isStable(tr.qualifier)
        case ThisType(tr: TypeRef) =>
          val flags = tr.typeSymbol.flags
          (flags.is(Flags.Module) || flags.is(Flags.Package)) && isStable(tr.qualifier)
        case NoPrefix() =>
          true
        case _ =>
          false
    isStable(tpe)

  private def isStableTypeRef(using qctx: Quotes)(tr: qctx.reflect.TypeRef): Boolean =
    import qctx.reflect.*
    val flags = tr.typeSymbol.flags
    !flags.is(Flags.Deferred) && !flags.is(Flags.Param) && isStableQualifier(tr.qualifier)

  def searchUnequal(
      using qctx: Quotes)(t1: qctx.reflect.TypeRepr, t2: qctx.reflect.TypeRepr): qctx.reflect.ImplicitSearchResult =
    import qctx.reflect.*
    Implicits.search(TypeRepr.of[!:=].appliedTo(List(t1, t2)))

  def areDifferentTypes(using qctx: Quotes)(t1: qctx.reflect.TypeRepr, t2: qctx.reflect.TypeRepr): Boolean =
    import qctx.reflect.*
    (t1.dealias.simplified, t2.dealias.simplified) match
      case (tr1: TypeRef, tr2: TypeRef) =>
        if isStableTypeRef(tr1) && isStableTypeRef(tr2) then !(t1 =:= t2)
        else false
      case (tr1: TypeRef, AppliedType(tcon2: TypeRef, _)) =>
        if isStableTypeRef(tr1) && isStableTypeRef(tcon2) then true
        else false
      case (AppliedType(tcon1: TypeRef, _), tr2: TypeRef) =>
        if isStableTypeRef(tcon1) && isStableTypeRef(tr2) then true
        else false
      case (AppliedType(tcon1: TypeRef, targs1), AppliedType(tcon2: TypeRef, targs2)) =>
        if isStableTypeRef(tcon1) && isStableTypeRef(tcon2) then
          if tcon1 =:= tcon2 then
            targs1.iterator.zip(targs2.iterator).foreach { case (targ1, targ2) =>
              searchUnequal(targ1, targ2) match
                case _: ImplicitSearchSuccess =>
                  return true
                case _: ImplicitSearchFailure =>
            }
            false
          else true
        else false
      case _ =>
        false

  def deriveUnequal[A: Type, B: Type](using qctx: Quotes): Expr[A !:= B] =
    import qctx.reflect.*
    areDifferentTypes(TypeRepr.of[A], TypeRepr.of[B]) match
      case true =>
        '{ (!:=).unsafeMake[A, B] }
      case false =>
        report.throwError(s"could not prove that types ${Type.show[A]} and ${Type.show[B]} are different")

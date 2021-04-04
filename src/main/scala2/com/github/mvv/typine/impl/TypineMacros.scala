package com.github.mvv.typine.impl

import com.github.mvv.typine.!:=

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object TypineMacros {
  sealed trait ComparedTypes
  object ComparedTypes {
    case object Same extends ComparedTypes
    case object Different extends ComparedTypes
    case object Unknown extends ComparedTypes
  }

  @tailrec
  private def isStableQualifier(c: blackbox.Context)(t: c.Type): Boolean = {
    import c.universe._
    t.dealias match {
      case TypeRef(pre, sym, List()) =>
        if (sym.isModule || sym.isPackage) {
          isStableQualifier(c)(pre)
        } else {
          false
        }
      case ThisType(sym) =>
        sym.isPackage
      case NoPrefix =>
        true
      case _ =>
        false
    }
  }

  def compareTypes(c: blackbox.Context)(t1: c.Type, t2: c.Type): ComparedTypes = {
    import c.universe._
    val TypeRef(ineqPre, ineqSym, _) = c.typeOf[!:=[Any, Any]]
    def recur(t1: Type, t2: Type): ComparedTypes =
      if (t1 =:= t2) {
        ComparedTypes.Same
      } else {
        (t1.dealias, t2.dealias) match {
          case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
            if (isStableQualifier(c)(pre1) && isStableQualifier(c)(pre2) && sym1.isClass && sym2.isClass) {
              if (pre1 =:= pre2 && sym1 == sym2) {
                args1.iterator.zip(args2.iterator).foreach {
                  case (arg1, arg2) =>
                    if (c.inferImplicitValue(c.internal.typeRef(ineqPre, ineqSym, List(arg1, arg2))) != EmptyTree) {
                      return ComparedTypes.Different
                    }
                }
                ComparedTypes.Unknown
              } else {
                ComparedTypes.Different
              }
            } else {
              ComparedTypes.Unknown
            }
          case _ =>
            ComparedTypes.Unknown
        }
      }
    recur(t1, t2)
  }

  def deriveUnequal[A, B](c: blackbox.Context)(implicit tag1: c.WeakTypeTag[A], tag2: c.WeakTypeTag[B]): c.Tree = {
    import c.universe._
    compareTypes(c)(tag1.tpe, tag2.tpe) match {
      case ComparedTypes.Different =>
        q"_root_.com.github.mvv.typine.!:=.unsafeMake[${tag1.tpe}, ${tag2.tpe}]"
      case ComparedTypes.Same =>
        c.abort(c.enclosingPosition, s"types ${tag1.tpe} and ${tag2.tpe} are the same")
      case ComparedTypes.Unknown =>
        c.abort(c.enclosingPosition, s"could not prove that types ${tag1.tpe} and ${tag2.tpe} are different")
    }
  }
}

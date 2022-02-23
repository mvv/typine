package com.github.mvv.typine.impl

import com.github.mvv.typine.!:=

import scala.annotation.tailrec
import scala.reflect.macros.blackbox

object TypineMacros {
  private def isStableQualifier(c: blackbox.Context)(t: c.Type): Boolean = {
    import c.universe._
    @tailrec
    def isStable(inner: c.Type): Boolean =
      inner.dealias match {
        case TypeRef(pre, sym, List()) =>
          (sym.isModule || sym.isPackage) && isStable(pre)
        case SingleType(pre, sym) =>
          if (sym.isModule || sym.isPackage) {
            isStable(pre)
          } else if (sym.isMethod) {
            sym.asTerm.typeSignature match {
              case NullaryMethodType(result) => isStable(result)
              case _                         => false
            }
          } else {
            sym.isTerm && isStable(sym.asTerm.typeSignature)
          }
        case ThisType(sym) =>
          sym.isPackage
        case NoPrefix =>
          true
        case _ =>
          false
      }
    isStable(t)
  }

  def searchUnequal(c: blackbox.Context)(t1: c.Type, t2: c.Type): c.Tree = {
    import c.universe._
    val TypeRef(ineqPre, ineqSym, _) = c.typeOf[!:=[Any, Any]]
    c.inferImplicitValue(c.internal.typeRef(ineqPre, ineqSym, List(t1, t2))) match {
      case q"com.github.mvv.typine.`package`.!:=.derive[$_, $_]" =>
      case tree if tree == EmptyTree                             =>
      case tree =>
        return tree
    }
    c.inferImplicitValue(c.internal.typeRef(ineqPre, ineqSym, List(t2, t1))) match {
      case q"com.github.mvv.typine.`package`.!:=.derive[$_, $_]" =>
        if (areDifferentTypes(c)(t1, t2)) {
          q"_root_.com.github.mvv.typine.!:=.unsafeMake[$t1, $t2]"
        } else {
          EmptyTree
        }
      case tree if tree == EmptyTree =>
        tree
      case tree =>
        q"$tree.flip"
    }
  }

  def areDifferentTypes(c: blackbox.Context)(t1: c.Type, t2: c.Type): Boolean = {
    import c.universe._
    (t1.dealias, t2.dealias) match {
      case (TypeRef(pre1, sym1, args1), TypeRef(pre2, sym2, args2)) =>
        if (isStableQualifier(c)(pre1) && isStableQualifier(c)(pre2) && sym1.isClass && sym2.isClass) {
          if (pre1 =:= pre2 && sym1 == sym2) {
            args1.iterator.zip(args2.iterator).foreach { case (arg1, arg2) =>
              if (searchUnequal(c)(arg1, arg2) != EmptyTree) {
                return true
              }
            }
            false
          } else {
            true
          }
        } else {
          false
        }
      case _ =>
        false
    }
  }

  def deriveUnequal[A, B](c: blackbox.Context)(implicit tag1: c.WeakTypeTag[A], tag2: c.WeakTypeTag[B]): c.Tree = {
    import c.universe._
    val TypeRef(ineqPre, ineqSym, _) = c.typeOf[!:=[Any, Any]]
    c.inferImplicitValue(c.internal.typeRef(ineqPre, ineqSym, List(tag2.tpe, tag1.tpe))) match {
      case q"com.github.mvv.typine.`package`.!:=.derive[$_, $_]" =>
      case tree =>
        if (tree != EmptyTree) {
          return q"$tree.flip"
        }
    }
    areDifferentTypes(c)(tag1.tpe, tag2.tpe) match {
      case true =>
        q"_root_.com.github.mvv.typine.!:=.unsafeMake[${tag1.tpe}, ${tag2.tpe}]"
      case false =>
        c.abort(c.enclosingPosition, s"could not prove that types ${tag1.tpe} and ${tag2.tpe} are different")
    }
  }
}

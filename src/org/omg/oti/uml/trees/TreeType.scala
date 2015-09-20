/*
 *
 *  License Terms
 *
 *  Copyright (c) 2015, California Institute of Technology ("Caltech").
 *  U.S. Government sponsorship acknowledged.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are
 *  met:
 *
 *
 *   *   Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *   *   Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the
 *       distribution.
 *
 *   *   Neither the name of Caltech nor its operating division, the Jet
 *       Propulsion Laboratory, nor the names of its contributors may be
 *       used to endorse or promote products derived from this software
 *       without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 *  IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 *  TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 *  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 *  OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 *  EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 *  PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.trees

import org.omg.oti.uml.read.api._
import scala.language.postfixOps

import scala.{Enumeration,Option,None,Some,StringContext,Tuple2}
import scala.Predef.{require,String}
import scala.collection.immutable._

/**
 * A UML Type (a kind of Class, except AssociationClass, or DataType) that
 * plays a structural role of some kind in the effective feature tree of
 * another TreeType.
 */
sealed trait TreeType[Uml <: UML] {

  /**
   * For SysML, treeFeatureType can be:
   * - a SysML Block (a UML Class with SysML's Block stereotype, or specialization of, applied)
   * - a SysML ValueType (a UML DataType with SysML's ValueType, or specialization of, applied)
   *
   * For UML, treeFeatureType can be:
   * - a UML Class (except AssociationClass) or specialization of (e.g., Component, Behavior, ...)
   * - a UML DataType or specialization of (e.g, PrimitiveType, Enumeration)
   */
  val treeFeatureType: UMLType[Uml]
}

sealed trait TreeTypeWithBranches[Uml <: UML]
  extends TreeType[Uml] {

  val branches: Seq[TreeFeatureBranch[Uml]]

}

case class TreeCompositeStructureType[Uml <: UML]
( override val treeFeatureType: UMLClass[Uml],
  override val branches: Seq[TreeFeatureBranch[Uml]])
  extends TreeTypeWithBranches[Uml] {

  import sext.PrettyPrinting._

  override def toString: String = this.treeString

}

case class TreeStructuredDataType[Uml <: UML]
( override val treeFeatureType: UMLDataType[Uml],
  override val branches: Seq[TreeFeatureBranch[Uml]])
  extends TreeTypeWithBranches[Uml] {

  import sext.PrettyPrinting._

  override def toString: String = this.treeString

}


/**
 * Limitations of structural analysis for UML Classifiers (StructuredClassifier or DataType)
 */
object IllFormedTreeTypeExplanation extends Enumeration {
  type IllFormedTreeTypeExplanation = Value

  /**
   * Indicates that the treeFeatureType has a UML TemplateSignature; that is,
   * the type is parametererized by the template parameters defined in the template signature.
   */
  val TemplateType = Value

  /**
   * Indicates that the treeFeatureType is parameterized in UML; that is,
   * the type (as a kind of UML TemplateableElement) has at least one UML TemplateBinding.
   */
  val ParameterizedType = Value

  /**
   * Indicates that the treeFeatureType is not a CompositeStructure or DataType
   */
  val NotCompositeStructureOrDataType = Value

  /**
   * Some features have conflicting names
   */
  val FeatureNameConflicts = Value
}

import IllFormedTreeTypeExplanation._

case class IllFormedTreeType[Uml <: UML]
( override val treeFeatureType: UMLType[Uml],
  explanation: Seq[IllFormedTreeTypeExplanation],
  nameConflicts: Map[String, Seq[TreeTypedFeatureBranch[Uml]]])
  extends TreeType[Uml] {

  import sext.PrettyPrinting._

  override def toString: String = this.treeString

}


object TreeType {

  /**
   * Collect ill-formed tree types and ill-formed tree type branches grouped by their owning tree type.
   *
   * @param treeType The root of the TreeType to collect ill-formed tree types & ill-formed tree type branches
   * @tparam Uml
   * @return A map with keys that are either:
   *         - an IllFormedTreeType, in which case there are no TreeFeatureBranches because the type tree is ill-formed.
   *         - a TreeTypedFeatureBranch, in which case there is at least one IllFormedTreeFeatureBranch.
   */
  def getIllFormedTreeBranches[Uml <: UML]
  ( treeType: TreeType[Uml])
  : Map[TreeType[Uml], Set[IllFormedTreeFeatureBranch[Uml]]] =
    ( Map[TreeType[Uml], Set[IllFormedTreeFeatureBranch[Uml]]]() /: getIllFormedTreeBranchPairs(treeType) ) {
      case ( acc, ( tt, ttb ) ) => acc.updated(tt, acc.getOrElse(tt, Set()) ++ ttb.toSet )
   }

  /**
   * Collect pairs of either ill-formed tree types or of a tree type and and ill-formed tree type branch.
   *
   * @param treeType The root of the TreeType to collect ill-formed tree types & ill-formed tree type branches
   * @tparam Uml
   * @return A sequence of pairs of one of two kinds:
   *         - an IllFormedTreeType with no IllFormedTreeFeatureBranch
   *         - a TreeTypedFeatureBranch with an IllFormedTreeFeatureBranch.
   */
  def getIllFormedTreeBranchPairs[Uml <: UML]
  ( treeType: TreeType[Uml])
  : Seq[(TreeType[Uml], Option[IllFormedTreeFeatureBranch[Uml]])] =
    treeType match {
      case tb: TreeTypeWithBranches[Uml] =>
        tb.branches flatMap TreeFeatureBranch.getIllFormedTreeBranchPairs(tb)
      case tb: IllFormedTreeType[Uml] =>
        Seq( Tuple2(tb, None) )
    }

  def makeTreeType[Uml <: UML]
  (treeFeatureType: UMLType[Uml])
  : Seq[TreeFeatureBranch[Uml]] => TreeType[Uml] =
    (branches: Seq[TreeFeatureBranch[Uml]]) =>
      treeFeatureType match {
        case treeFeatureClass: UMLClass[Uml] => TreeCompositeStructureType(treeFeatureClass, branches)
        case treeFeatureDataType: UMLDataType[Uml] => TreeStructuredDataType(treeFeatureDataType, branches)
      }

  def makeTreeCompositeStructureType[Uml <: UML]
  (treeFeatureType: UMLClass[Uml])
  : Seq[TreeFeatureBranch[Uml]] => TreeCompositeStructureType[Uml] =
    (branches: Seq[TreeFeatureBranch[Uml]]) => TreeCompositeStructureType(treeFeatureType, branches)

  def makeTreeDataType[Uml <: UML]
  (treeFeatureType: UMLDataType[Uml])
  : Seq[TreeFeatureBranch[Uml]] => TreeStructuredDataType[Uml] =
    (branches: Seq[TreeFeatureBranch[Uml]]) => TreeStructuredDataType(treeFeatureType, branches)

}
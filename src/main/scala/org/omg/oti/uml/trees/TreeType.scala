/*
 * Copyright 2014 California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * Copyright 2015 Airbus.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * License Terms
 */

package org.omg.oti.uml.trees

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._

import scala.{Enumeration,Option,None,Tuple2}
import scala.Predef.String
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
  nameConflicts: Map[String, Seq[TreeTypedFeatureBranch[Uml]]],
  override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException("IllFormedTreeType", cause)
  with TreeType[Uml] {

  import sext.PrettyPrinting._

  override val message: String =
    this.toString

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
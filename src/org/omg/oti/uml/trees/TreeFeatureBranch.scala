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
import org.omg.oti.uml.xmi.IDGenerator

/**
 * For a UML composite structure (class, but not a kind of association class) or UML DataType,
 * each feature that plays a role in the structure of the composite has a corresponding TreeFeatureBranch.
 */
sealed trait TreeFeatureBranch[Uml <: UML] {

  /**
   * branch can be a UML Property or a Port, not a Connector
   */
  val branch: Option[UMLStructuralFeature[Uml]]
  val association: Option[UMLAssociation[Uml]] = None

  require(branch.isDefined || association.isDefined)
}

sealed trait TreeTypedFeatureBranch[Uml <: UML]
  extends TreeFeatureBranch[Uml] {

  override val branch: Some[UMLProperty[Uml]]

  /**
   * child
   */
  val child: TreeType[Uml]

  require(branch.get.name.isDefined)

  lazy val name: String = branch.get.name.get

}

case class TreePropertyBranch[Uml <: UML]
( override val branch: Some[UMLProperty[Uml]],
  override val child: TreeType[Uml])
  extends TreeTypedFeatureBranch[Uml] {

  import sext._

  override def toString: String = this.valueTreeString

}

case class TreeAssociationPropertyBranch[Uml <: UML]
( override val branch: Some[UMLProperty[Uml]],
  override val association: Some[UMLAssociation[Uml]],
  override val child: TreeType[Uml] )
  extends TreeTypedFeatureBranch[Uml] {
  import sext._

  override lazy val name: String = branch.get.owningAssociation match {
    case Some(a) =>
      require(a == association.get)
      a.name.get+"."+branch.get.name.get
    case None =>
      branch.get.name.get
  }

  override def toString: String = this.treeString
}

case class TreePortBranch[Uml <: UML]
( override val branch: Some[UMLPort[Uml]],
  override val child: TreeType[Uml])
  extends TreeTypedFeatureBranch[Uml] {

  import sext._

  override def toString: String = this.treeString
}

case class TreeAssociationPortBranch[Uml <: UML]
( override val branch: Some[UMLPort[Uml]],
  override val association: Some[UMLAssociation[Uml]],
  override val child: TreeType[Uml])
  extends TreeTypedFeatureBranch[Uml] {

  import sext._

  override def toString: String = this.treeString
}

/**
 * Limitations of structural analysis for UML "structural" features
 */
object IllFormedTreeFeatureExplanation extends Enumeration {
  type IllFormedTreeFeatureExplanation = Value

  /**
   * Indicates that the structural feature lacks a name.
   */
  val UnnamedStructuralFeature = Value

  /**
   * Indicates that the structural feature has a lower multiplicity bound of 0.
   * This means that this feature is optional in the context of its classifier.
   * A TreeFeatureBranch must be unary, i.e., always present in every well-formed instance of its owning class.
   * A feature with optional multiplicity results in an under-specified model of tree structures
   * because there is no specification in UML for the difference where the optional feature is
   * present (i.e., there is a value) vs. not present (i.e., there is no value).
   */
  val OptionalMultiplicity = Value

  /**
   * Indicates that the structural feature has an upper multiplicity bound of 0.
   * This means that this feature has no value in the context of its classifier.
   * Not supported (yet).
   */
  val ZeroMultiplicity = Value

  /**
   * Indicates that the structural feature has a lower or upper multiplicity bound > 1.
   * This means tha this feature models a collection of values.
   * A TreeFeatureBranch must be unary, i.e., representing a distinct use of its type.
   * A feature with collection multiplicity results in an under-specified model of tree structures
   * because there is no specification in UML for the role that different values in that collection have.
   */
  val CollectionMultiplicity = Value

  /** Indicates that the property has no type specified.
    * An untyped structural feature results in an under-specified model of tree structures
    * because there is no constraints in UML for the possible roles that an untyped feature may have.
    * Such an under-constrained model could allow for structures that are not trees, e.g., circular graphs.
    */
  val UntypedProperty = Value

  /** Indicates that the port has no type specified.
    * An untyped structural feature results in an under-specified model of tree structures
    * because there is no constraints in UML for the possible roles that an untyped feature may have.
    * Such an under-constrained model could allow for structures that are not trees, e.g., circular graphs.
    */
  val UntypedPort = Value

  /**
   * Indicates that there is a circular structure from the type of the feature, T1,
   * involving at least one other type, T2, playing a structural feature role for T1,
   * such that T2 either directly or indirectly has a structural feature typed by T1.
   */
  val CircularTopology = Value

  /**
   * Indicates that a UML DataType has a Port feature.
   */
  val DataTypePort = Value

  /**
   * Indicates that there is no branch for an undirected binary association
   */
  val UndirectedBinaryAssociation = Value

  /**
   * Indicates that there is no branch for a n-ary association (n>2)
   */
  val NaryAssociation = Value

  /**
   * The 'from' memberEnd of the binary directed association is untyped.
   */
  val UntypedAssociationFromMemberEnd = Value

  /**
   * The 'from' memberEnd of the binary directed association has a different
   * type than the context.
   */
  val UnrelatedAssociationFromMemberEndType = Value

  /**
   * The 'to' memberEnd of the binary directed association is untyped.
   */
  val UntypedAssociationToMemberEnd = Value

}

import IllFormedTreeFeatureExplanation._

case class IllFormedTreeFeatureBranch[Uml <: UML]
( override val branch: Option[UMLStructuralFeature[Uml]],
  override val association: Option[UMLAssociation[Uml]],
  explanation: Seq[IllFormedTreeFeatureExplanation])
  extends TreeFeatureBranch[Uml] {

  import sext._

  override def toString: String = this.treeString
}

object TreeFeatureBranch {

  /**
   * Collect pairs of either ill-formed tree types or of a tree type and and ill-formed tree type branch.
   *
   * @param treeType The parent TreeType for a TreeFeatureBranch to analyze
   * @param treeBranch A TreeFeatureBranch for which to collect pairs
   * @tparam Uml
   * @return A sequence of pairs of one of two kinds:
   *         - an IllFormedTreeType with no IllFormedTreeFeatureBranch
   *         - a TreeTypedFeatureBranch with an IllFormedTreeFeatureBranch.
   */
  def getIllFormedTreeBranchPairs[Uml <: UML]
  ( treeType: TreeType[Uml] )
  ( treeBranch: TreeFeatureBranch[Uml] )
  : Seq[(TreeType[Uml], Option[IllFormedTreeFeatureBranch[Uml]])] =
    treeBranch match {
      case ttb: TreeTypedFeatureBranch[Uml] =>
        TreeType.getIllFormedTreeBranchPairs(ttb.child)
      case ttb: IllFormedTreeFeatureBranch[Uml] =>
        Seq( Tuple2(treeType, Some(ttb)) )
    }

  def makeTreeAssociationPropertyBranch[Uml <: UML]
  (branch: UMLProperty[Uml], association: UMLAssociation[Uml])
  : (TreeType[Uml] => TreeFeatureBranch[Uml]) =
    (child: TreeType[Uml]) => TreeAssociationPropertyBranch(Some(branch), Some(association), child)

  def makeTreeAssociationPortBranch[Uml <: UML]
  (branch: UMLPort[Uml], association: UMLAssociation[Uml])
  : (TreeType[Uml] => TreeFeatureBranch[Uml]) =
    (child: TreeType[Uml]) => TreeAssociationPortBranch(Some(branch), Some(association), child)

  def treeFeatureBranchOrdering[Uml <: UML]
  ()
  (implicit idg: IDGenerator[Uml])
  : Ordering[TreeFeatureBranch[Uml]] =
    new Ordering[TreeFeatureBranch[Uml]]() {

    def compare( x: TreeFeatureBranch[Uml], y: TreeFeatureBranch[Uml] ): Int = {
      require(x.branch.isDefined || x.association.isDefined)
      require(y.branch.isDefined || y.association.isDefined)
      ((x.branch, x.association, y.branch, y.association) : @unchecked) match {
        case (Some(px), _, Some(py), _) =>
          (px.name, py.name) match {
            case (Some(nx), Some(ny)) =>
              nx.compareTo(ny)
            case (_, _) =>
              px.xmiID().compareTo(py.xmiID())
          }
        case (Some(px), _, None, Some(ay)) =>
          px.xmiID().compareTo(ay.xmiID())
        case (None, Some(ax), Some(py), _) =>
          ax.xmiID().compareTo(py.xmiID())
        case (None, Some(ax), None, Some(ay)) =>
          ax.xmiID().compareTo(ay.xmiID())
      }
    }

  }

}
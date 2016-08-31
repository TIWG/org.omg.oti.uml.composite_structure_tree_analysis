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

import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.uml.UMLError
import org.omg.oti.uml.characteristics._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.IDGenerator

import scala.{Enumeration,Option,None,Some,Tuple2}
import scala.Predef.{require,String}
import scala.collection.immutable._
import scalaz._, Scalaz._
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

  import sext.PrettyPrinting._

  override def toString: String =
    this.valueTreeString

}

case class TreeAssociationPropertyBranch[Uml <: UML]
( override val branch: Some[UMLProperty[Uml]],
  override val association: Some[UMLAssociation[Uml]],
  override val child: TreeType[Uml] )
  extends TreeTypedFeatureBranch[Uml] {
  import sext.PrettyPrinting._

  override lazy val name: String =
    branch
    .get.owningAssociation
    .fold[String]( ifEmpty = branch.get.name.getOrElse("") ) { a =>
      require(a == association.get)
      a.name.get+"."+branch.get.name.get
    }

  override def toString: String = this.treeString
}

case class TreePortBranch[Uml <: UML]
( override val branch: Some[UMLPort[Uml]],
  override val child: TreeType[Uml])
  extends TreeTypedFeatureBranch[Uml] {

  import sext.PrettyPrinting._

  override def toString: String =
    this.treeString
}

case class TreeAssociationPortBranch[Uml <: UML]
( override val branch: Some[UMLPort[Uml]],
  override val association: Some[UMLAssociation[Uml]],
  override val child: TreeType[Uml])
  extends TreeTypedFeatureBranch[Uml] {

  import sext.PrettyPrinting._

  override def toString: String =
    this.treeString
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
  explanation: Seq[IllFormedTreeFeatureExplanation],
  override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  extends UMLError.UException("IllFormedTreeFeatureBranch", cause)
  with TreeFeatureBranch[Uml] {

  import sext.PrettyPrinting._

  override val message: String =
    this.toString

  override def toString: String =
    this.treeString
}

object TreeFeatureBranch {

  implicit def TreeFeatureBranchSeqSemigroup[Uml <: UML]: Semigroup[Seq[TreeFeatureBranch[Uml]]] =
    Semigroup.instance(_ ++ _)


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
  (implicit
   idg: IDGenerator[Uml],
   otiCharacteristicsProvider: OTICharacteristicsProvider[Uml])
  : Order[TreeFeatureBranch[Uml]] =
    Order[String].contramap[TreeFeatureBranch[Uml]]( (x: TreeFeatureBranch[Uml]) => {
      (x.branch, x.association) match {
        case (Some(sf), _) =>
          sf.xmiID().toOption.map(OTI_ID.unwrap).getOrElse(sf.name.getOrElse(""))
        case (None, Some(a)) =>
          a.xmiID().toOption.map(OTI_ID.unwrap).getOrElse(a.name.getOrElse(""))
        case (_, _) =>
          require(false, "branch or association must be defined")
          ""
      }
    })

}
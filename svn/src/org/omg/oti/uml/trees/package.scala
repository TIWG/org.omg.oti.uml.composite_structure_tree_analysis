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

package org.omg.oti.uml

import org.omg.oti.json.common.OTIPrimitiveTypes._
import org.omg.oti.uml.characteristics._
import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.IDGenerator

import scala.{Boolean,Option,None,Some}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scalaz._, Scalaz._

/**
 * Extension of OMG UML CompositeStructure with SysML PropertySpecificType and BlockSpecificType
 *
 * The main objective of OTI's tree analysis is to support the subtle but important
 * distinction between
 *   - the open-world semantics of UML Class & SysML Block Definition Diagrams
 *   - the closed-world semantics of UML Composite Structure Diagrams,
 *     SysML Internal Block Diagrams & SysML Parametric Diagrams.
 */
package object trees {

  def treeOpsException[Uml <: UML]
  (treeOps: TreeOps[Uml],
   message: String,
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new TreeOpsException(treeOps, message, cause)

  def treeOpsException[Uml <: UML]
  (treeOps: TreeOps[Uml],
   message: String,
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new TreeOpsException(treeOps, message, Set(cause).some)

  def illFormedTreeType[Uml <: UML]
  (treeFeatureType: UMLType[Uml],
   explanation: Seq[IllFormedTreeTypeExplanation.Value],
   nameConflicts: Map[String, Seq[TreeTypedFeatureBranch[Uml]]],
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new IllFormedTreeType(treeFeatureType, explanation, nameConflicts, cause)

  def illFormedTreeType[Uml <: UML]
  (treeFeatureType: UMLType[Uml],
   explanation: Seq[IllFormedTreeTypeExplanation.Value],
   nameConflicts: Map[String, Seq[TreeTypedFeatureBranch[Uml]]],
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new IllFormedTreeType(treeFeatureType, explanation, nameConflicts, Set(cause).some)

  def illFormedTreeFeatureBranch[Uml <: UML]
  (branch: Option[UMLStructuralFeature[Uml]],
   association: Option[UMLAssociation[Uml]],
   explanation: Seq[IllFormedTreeFeatureExplanation.Value],
   cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
  : java.lang.Throwable =
    new IllFormedTreeFeatureBranch(branch, association, explanation, cause)

  def illFormedTreeFeatureBranch[Uml <: UML]
  (branch: Option[UMLStructuralFeature[Uml]],
   association: Option[UMLAssociation[Uml]],
   explanation: Seq[IllFormedTreeFeatureExplanation.Value],
   cause: java.lang.Throwable)
  : java.lang.Throwable =
    new IllFormedTreeFeatureBranch(branch, association, explanation, Set(cause).some)

  def analyze[Uml <: UML]
  (t: UMLType[Uml])
  (implicit
   treeOps: TreeOps[Uml],
   idg: IDGenerator[Uml],
   otiCharacteristicsProvider: OTICharacteristicsProvider[Uml])
  : Set[java.lang.Throwable] \/ TreeType[Uml] =
    trees.analyze(Seq(), t)

  def analyze[Uml <: UML]
  (treePath: Seq[UMLType[Uml]], t: UMLType[Uml])
  (implicit
   treeOps: TreeOps[Uml],
   idg: IDGenerator[Uml],
   otiCharacteristicsProvider: OTICharacteristicsProvider[Uml])
  : Set[java.lang.Throwable] \/ TreeType[Uml] =
    t match {
      case ta: UMLAssociationClass[Uml] =>
        analyzeBranches(treePath, ta)
      case tc: UMLClass[Uml] =>
        analyzeBranches(treePath, tc)
      case td: UMLDataType[Uml] =>
        analyzeBranches(treePath, td)
      case _t =>
        Set(
          illFormedTreeType(_t, Seq(IllFormedTreeTypeExplanation.NotCompositeStructureOrDataType), Map())
        ).left
    }

  def acyclicTypes[Uml <: UML]
  (treePath: Seq[UMLType[Uml]], context: UMLType[Uml])
  : Boolean =
    treePath.forall(t => !t.conformsTo(Some(context)) && !context.conformsTo(Some(t)))

  def analyzeBranches[Uml <: UML]
  (treePath: Seq[UMLType[Uml]], treeContext: UMLClassifier[Uml])
  (implicit
   treeOps: TreeOps[Uml],
   idg: IDGenerator[Uml],
   otiCharacteristicsProvider: OTICharacteristicsProvider[Uml])
  : Set[java.lang.Throwable] \/ TreeType[Uml] = {

    implicit def UMLPropertySeqSemigroup: Semigroup[Seq[UMLProperty[Uml]]] =
      Semigroup.instance(_ ++ _)

    val associationBranches: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] = {

      val associations = treeContext
        .endType_associationExceptRedefinedOrDerived
        .toList.sortBy(_.xmiID().toOption.map(OTI_ID.unwrap).getOrElse("")) // @todo propagate errors

      val a0: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] = Seq().right
      val aN: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] =
        (a0 /: associations) { (ai, a) =>

        val inc: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] =
        if (a.memberEnd.size > 2)
          Set(
            illFormedTreeFeatureBranch(None, Some(a), Seq(IllFormedTreeFeatureExplanation.NaryAssociation))
          ).left
        else
          a
          .getDirectedAssociationEnd
          .fold[\/[Set[java.lang.Throwable], Seq[TreeFeatureBranch[Uml]]]]{
            Set(
              illFormedTreeFeatureBranch(None, Some(a), Seq(IllFormedTreeFeatureExplanation.UndirectedBinaryAssociation))
            ).left
          }{
             case (aFrom, aTo)
              if treeContext.conformsTo(aFrom._type) =>
              val err_dataTypePort = (treeContext, aTo) match {
                case (_: UMLDataType[Uml], _: UMLPort[Uml]) =>
                  Seq(IllFormedTreeFeatureExplanation.DataTypePort)
                case _ =>
                  Seq()
              }
              val err_toType = aTo._type match {
                case None =>
                  Seq(IllFormedTreeFeatureExplanation.UntypedAssociationToMemberEnd)
                case Some(tTo) =>
                  if (acyclicTypes(treePath, treeContext) && acyclicTypes(treePath :+ treeContext, tTo))
                    Seq()
                  else
                    Seq(IllFormedTreeFeatureExplanation.CircularTopology)
              }
              val err_toLower = aTo.lower.intValue match {
                case 0 =>
                  Seq(IllFormedTreeFeatureExplanation.OptionalMultiplicity)
                case l =>
                  if (l == 1)
                    Seq()
                  else
                    Seq(IllFormedTreeFeatureExplanation.CollectionMultiplicity)
              }
              val err_toUpper = aTo.upper.intValue match {
                case 0 =>
                  Seq(IllFormedTreeFeatureExplanation.ZeroMultiplicity)
                case l =>
                  if (l == 1)
                    Seq()
                  else
                    Seq(IllFormedTreeFeatureExplanation.CollectionMultiplicity)
              }
              val err_toNamed = aTo.name match {
                case Some(_) =>
                  Seq()
                case None =>
                  Seq(IllFormedTreeFeatureExplanation.UnnamedStructuralFeature)
              }
              err_dataTypePort.toList ++
                err_toType.toList ++
                err_toLower.toList ++
                err_toUpper.toList ++
                err_toNamed.toList match {
                case Nil =>
                  require(aTo._type.isDefined)
                  aTo match {
                    case aToPort: UMLPort[Uml] =>
                      trees.analyze(treePath :+ treeContext, aTo._type.get)
                      .map { tree =>
                        Seq(TreeAssociationPortBranch(
                          Some(aToPort), Some(a), tree))
                      }
                    case aToProp: UMLProperty[Uml] =>
                      trees.analyze(treePath :+ treeContext, aTo._type.get)
                      .map { tree =>
                        Seq(TreeAssociationPropertyBranch(
                          Some(aToProp), Some(a), tree))
                      }
                  }
                case problems =>
                    Set(
                      illFormedTreeFeatureBranch(Some(aTo), Some(a), problems.toSeq)
                    ).left
              }

            case (aFrom, aTo)
              if treeContext.conformsTo(aTo._type) =>
              Seq().right

            case (aFrom, aTo) =>
              aFrom
                ._type
                .fold[\/[Set[java.lang.Throwable], Seq[TreeFeatureBranch[Uml]]]](
                  Set(
                    illFormedTreeFeatureBranch(None, Some(a),
                      Seq(IllFormedTreeFeatureExplanation.UntypedAssociationFromMemberEnd))
                  ).left
              ){ tFrom =>
                Set(
                  illFormedTreeFeatureBranch(None, Some(a),
                    Seq(IllFormedTreeFeatureExplanation.UnrelatedAssociationFromMemberEndType))
                ).left
              }
          }

        ai +++ inc
      }

      aN
    }

    val nonAssociationBranches: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] = {

      val properties0: Set[java.lang.Throwable] \/ Seq[UMLProperty[Uml]] = Seq().right
      val propertiesN = (properties0 /: treeContext.allAttributesExceptRedefined) { (propertiesI, p) =>
        propertiesI +++
        treeOps.hasClosedWorldInterpretation(treeContext, p).map { cwi: Boolean =>
          if (p.association.isEmpty && p.aggregation.contains(UMLAggregationKind.composite) && cwi)
            Seq(p)
          else
            Seq()
        }
      }

      propertiesN
      .flatMap { properties: Seq[UMLProperty[Uml]] =>
        val p0: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] = Seq().right
        val pN: Set[java.lang.Throwable] \/ Seq[TreeFeatureBranch[Uml]] =
          (p0 /: properties) { (pi, p) =>

            val err_dataTypePort = (treeContext, p) match {
              case (_: UMLDataType[Uml], _: UMLPort[Uml]) =>
                Some(IllFormedTreeFeatureExplanation.DataTypePort)
              case _ =>
                None
            }
            val err_pType = p._type match {
              case None =>
                p match {
                  case _: UMLPort[Uml] =>
                    Some(IllFormedTreeFeatureExplanation.UntypedPort)
                  case _ =>
                    Some(IllFormedTreeFeatureExplanation.UntypedProperty)
                }
              case Some(t) =>
                if (acyclicTypes(treePath :+ treeContext, t))
                  None
                else
                  Some(IllFormedTreeFeatureExplanation.CircularTopology)
            }
            val err_pLower = p.lower.intValue match {
              case 0 =>
                Some(IllFormedTreeFeatureExplanation.OptionalMultiplicity)
              case l =>
                if (l == 1) None
                else Some(IllFormedTreeFeatureExplanation.CollectionMultiplicity)
            }
            val err_pUpper = p.upper.intValue match {
              case 0 =>
                Some(IllFormedTreeFeatureExplanation.ZeroMultiplicity)
              case l =>
                if (l == 1)
                  None
                else
                  Some(IllFormedTreeFeatureExplanation.CollectionMultiplicity)
            }
            val err_pNamed = p.name match {
              case Some(_) => None
              case None => Some(IllFormedTreeFeatureExplanation.UnnamedStructuralFeature)
            }

            val inc =
              err_dataTypePort.toList ++
                err_pType.toList ++
                err_pLower.toList ++
                err_pUpper.toList ++
                err_pNamed.toList match {
                case Nil =>
                  require(p._type.isDefined)
                  p match {
                    case port: UMLPort[Uml] =>
                      trees.analyze(treePath :+ treeContext, p._type.get)
                        .map { tree =>
                          Seq(TreePortBranch(Some(port), tree))
                        }
                    case prop: UMLProperty[Uml] =>
                      trees.analyze(treePath :+ treeContext, p._type.get)
                        .map { tree =>
                          Seq(TreePropertyBranch(Some(prop), tree))
                        }
                  }
                case problems =>
                  Set(
                    illFormedTreeFeatureBranch(Some(p), None, problems.toSeq)
                  ).left
              }

            pi +++ inc
          }

        pN
      }
    }

    (associationBranches +++ nonAssociationBranches)
    .flatMap[Set[java.lang.Throwable], TreeType[Uml]]{
      (allBranches: Seq[TreeFeatureBranch[Uml]]) => 

        val allTypedBranches: Seq[TreeTypedFeatureBranch[Uml]] =
          allBranches.flatMap {
            case b: TreeTypedFeatureBranch[Uml] =>
              Some(b)
            case _ =>
              None
          }

        val nameConflicts = allTypedBranches.groupBy(_.name).filter(_._2.size > 1)

        if (nameConflicts.isEmpty)
          \/-(TreeType.makeTreeType(treeContext)(allBranches))
        else
          Set(
            illFormedTreeType(treeContext, Seq(IllFormedTreeTypeExplanation.FeatureNameConflicts), nameConflicts)
          ).left
    }
  }
}
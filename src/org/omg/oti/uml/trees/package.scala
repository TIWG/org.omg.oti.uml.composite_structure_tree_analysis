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
package org.omg.oti.uml

import org.omg.oti.uml.read.api._
import org.omg.oti.uml.xmi.IDGenerator

import scala.{Boolean,Option,None,Some,StringContext}
import scala.Predef.{Set => _, Map => _, _}
import scala.collection.immutable._
import scala.language.postfixOps
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

  def analyze[Uml <: UML]
  (t: UMLType[Uml])
  (implicit treeOps: TreeOps[Uml], idg: IDGenerator[Uml])
  : ValidationNel[UMLError.UException, TreeType[Uml]] =
    trees.analyze(Seq(), t)

  def analyze[Uml <: UML]
  (treePath: Seq[UMLType[Uml]], t: UMLType[Uml])
  (implicit treeOps: TreeOps[Uml], idg: IDGenerator[Uml])
  : ValidationNel[UMLError.UException, TreeType[Uml]] =
    t match {
      case ta: UMLAssociationClass[Uml] =>
        analyzeBranches(treePath, ta)
      case tc: UMLClass[Uml] =>
        analyzeBranches(treePath, tc)
      case td: UMLDataType[Uml] =>
        analyzeBranches(treePath, td)
      case _t =>
        IllFormedTreeType(_t, Seq(IllFormedTreeTypeExplanation.NotCompositeStructureOrDataType), Map()).failureNel
    }

  def acyclicTypes[Uml <: UML]
  (treePath: Seq[UMLType[Uml]], context: UMLType[Uml])
  : Boolean =
    treePath.forall(t => !t.conformsTo(Some(context)) && !context.conformsTo(Some(t)))

  def analyzeBranches[Uml <: UML]
  (treePath: Seq[UMLType[Uml]], treeContext: UMLClassifier[Uml])
  (implicit treeOps: TreeOps[Uml], idg: IDGenerator[Uml])
  : ValidationNel[UMLError.UException, TreeType[Uml]] = {

    val associationBranches: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] = {

      val associations = treeContext
        .endType_associationExceptRedefinedOrDerived
        .toList.sortBy(_.xmiID().toOption.getOrElse("")) // @todo propagate errors

      val a0: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] = Seq().success
      val aN: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] =
        (a0 /: associations) { (ai, a) =>

        val inc: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] =
        if (a.memberEnd.size > 2)
          IllFormedTreeFeatureBranch(None, Some(a), Seq(IllFormedTreeFeatureExplanation.NaryAssociation)).failureNel
        else
          a
          .getDirectedAssociationEnd
          .fold[ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]]]{
            IllFormedTreeFeatureBranch(None, Some(a), Seq(IllFormedTreeFeatureExplanation.UndirectedBinaryAssociation)).failureNel
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
                  IllFormedTreeFeatureBranch(Some(aTo), Some(a), problems.toSeq).failureNel
              }

            case (aFrom, aTo)
              if treeContext.conformsTo(aTo._type) =>
              Seq().successNel

            case (aFrom, aTo) =>
              aFrom._type match {
                case None =>
                  IllFormedTreeFeatureBranch(None, Some(a),
                    Seq(IllFormedTreeFeatureExplanation.UntypedAssociationFromMemberEnd)).failureNel
                case Some(tFrom) =>
                  IllFormedTreeFeatureBranch(None, Some(a),
                    Seq(IllFormedTreeFeatureExplanation.UnrelatedAssociationFromMemberEndType)).failureNel
              }
          }

        ai +++ inc
      }

      aN
    }

    val nonAssociationBranches: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] = {

      val properties = treeContext.allAttributesExceptRedefined.filter { p =>
        p.association.isEmpty &&
          p.aggregation.contains(UMLAggregationKind.composite) &&
          treeOps.hasClosedWorldInterpretation(treeContext, p)
      }

      val p0: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] = Seq().success
      val pN: ValidationNel[UMLError.UException, Seq[TreeFeatureBranch[Uml]]] =
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
                .map{ tree =>
                  Seq(TreePortBranch(Some(port), tree))
                }
              case prop: UMLProperty[Uml] =>
                trees.analyze(treePath :+ treeContext, p._type.get)
                .map { tree =>
                  Seq(TreePropertyBranch(Some(prop), tree))
                }
            }
          case problems =>
            IllFormedTreeFeatureBranch(Some(p), None, problems.toSeq).failureNel
        }

        pi +++ inc
      }

      pN
    }

    (associationBranches +++ nonAssociationBranches)
      .disjunction
      .flatMap[NonEmptyList[UMLError.UException], TreeType[Uml]]( (allBranches: Seq[TreeFeatureBranch[Uml]]) => {

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
          -\/(IllFormedTreeType(treeContext, Seq(IllFormedTreeTypeExplanation.FeatureNameConflicts), nameConflicts).wrapNel)
    }).validation

  }
}
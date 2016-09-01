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

import scala.Boolean
import scala.Predef.String
import scala.collection.immutable.Set
import scalaz._

class TreeOpsException[Uml <: UML]
(treeOps: TreeOps[Uml],
 override val message: String,
 override val cause: UMLError.OptionThrowableNel = UMLError.emptyThrowableNel)
extends UMLError.UException(message, cause) {

  /**
   * This type member is intended to facilitate pattern matching
   * using a wildcard for the type parameter, i.e., TreeOpsException[_]
   * The type information can then be checked using the UmlType member.
   */
  type UmlType = Uml
}

/**
 * Extension of UML Composite Structure with SysML's PropertySpecificType and JPL's proposal for SysML BlockSpecificType
 *
 * @tparam Uml The type signature for a tool-specific adaptation of the OTI UML API
 */
trait TreeOps[Uml <: UML] {

  /**
   * Predicate for the open-world (false) vs. closed-world(true) interpretation of the features
   * (structural, behavioral, connector) described in the context of a SysML Block as the root
   * type of a system description.
   *
   * @param treeType The SysML Block context to check for an open vs. closed world interpretation
   * @return
   */
  def isRootBlockSpecificType(treeType: UMLClassifier[Uml])
  : Set[java.lang.Throwable] \/ Boolean

  /**
   * Predicate for the open-world (false) vs. closed-world(true) interpretation of the features
   * (structural, behavioral, connector) described in the context of a SysML Block as the type
   * of a part in a system description.
   *
   * @param treeType The SysML Block context to check for an open vs. closed world interpretation
   * @return
   */
  def isPartPropertySpecificType(treeType: UMLClassifier[Uml])
  : Set[java.lang.Throwable] \/ Boolean

  def hasClosedWorldInterpretation(treeType: UMLClassifier[Uml], p: UMLProperty[Uml])
  : Set[java.lang.Throwable] \/ Boolean =
    p
    ._type
    .fold[\/[Set[java.lang.Throwable], Boolean]](\/-(false)) {
      case featureType: UMLClassifier[Uml] =>
        for {
          treeTypeIsRootBST <- isRootBlockSpecificType(treeType)
          treeTypeIsPartPST <- isPartPropertySpecificType(treeType)
          featureTypeIsPartPST <- isPartPropertySpecificType(featureType)
        } yield
          (treeTypeIsRootBST || treeTypeIsPartPST) && featureTypeIsPartPST ||
          !treeTypeIsRootBST && !treeTypeIsPartPST && !featureTypeIsPartPST

      case _ =>
        \/-(false)
    }
}
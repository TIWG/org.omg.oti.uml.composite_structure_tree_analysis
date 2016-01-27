/*
 *
 * License Terms
 *
 * Copyright (c) 2014-2016, California Institute of Technology ("Caltech").
 * U.S. Government sponsorship acknowledged.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 *
 * *   Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * *   Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the
 *    distribution.
 *
 * *   Neither the name of Caltech nor its operating division, the Jet
 *    Propulsion Laboratory, nor the names of its contributors may be
 *    used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package org.omg.oti.uml.trees

import org.omg.oti.uml.UMLError
import org.omg.oti.uml.read.api._

import scala.Boolean
import scala.Predef.String
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
  : NonEmptyList[java.lang.Throwable] \/ Boolean

  /**
   * Predicate for the open-world (false) vs. closed-world(true) interpretation of the features
   * (structural, behavioral, connector) described in the context of a SysML Block as the type
   * of a part in a system description.
   *
   * @param treeType The SysML Block context to check for an open vs. closed world interpretation
   * @return
   */
  def isPartPropertySpecificType(treeType: UMLClassifier[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Boolean

  def hasClosedWorldInterpretation(treeType: UMLClassifier[Uml], p: UMLProperty[Uml])
  : NonEmptyList[java.lang.Throwable] \/ Boolean =
    p
    ._type
    .fold[\/[NonEmptyList[java.lang.Throwable], Boolean]](\/-(false)) {
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
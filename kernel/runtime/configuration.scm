;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Configuration
;;;
;;;  The contents of this file are subject to the Mozilla Public License Version
;;;  1.1 (the "License"); you may not use this file except in compliance with
;;;  the License. You may obtain a copy of the License at
;;;  http://www.mozilla.org/MPL/
;;;
;;;  Software distributed under the License is distributed on an "AS IS" basis,
;;;  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
;;;  for the specific language governing rights and limitations under the
;;;  License.
;;;
;;;  The Original Code is JazzScheme.
;;;
;;;  The Initial Developer of the Original Code is Guillaume Cartier.
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
;;;  the Initial Developer. All Rights Reserved.
;;;
;;;  Contributor(s):
;;;
;;;  Alternatively, the contents of this file may be used under the terms of
;;;  the GNU General Public License Version 2 or later (the "GPL"), in which
;;;  case the provisions of the GPL are applicable instead of those above. If
;;;  you wish to allow use of your version of this file only under the terms of
;;;  the GPL, and not to allow others to use your version of this file under the
;;;  terms of the MPL, indicate your decision by deleting the provisions above
;;;  and replace them with the notice and other provisions required by the GPL.
;;;  If you do not delete the provisions above, a recipient may use your version
;;;  of this file under the terms of any one of the MPL or the GPL.
;;;
;;;  See www.jazzscheme.org for details.


(block kernel.configuration


;;;
;;;; Source
;;;


(define jazz:jazz-source
  jazz:source)


;;;
;;;; Configuration
;;;


(jazz:define-structure Configuration () (constructor: jazz:make-configuration)
  ((name                getter: generate)
   (system              getter: generate)
   (platform            getter: generate)
   (compiler            getter: generate)
   (processor           getter: generate)
   (windowing           getter: generate)
   (safety              getter: generate)
   (optimize?           getter: generate)
   (debug-environments? getter: generate)
   (debug-location?     getter: generate)
   (debug-source?       getter: generate)
   (debug-foreign?      getter: generate)
   (track-memory?       getter: generate)
   (mutable-bindings?   getter: generate)
   (kernel-interpret?   getter: generate)
   (destination         getter: generate)
   (features            getter: generate)
   (properties          getter: generate)
   (local?              getter: generate)))


(define (jazz:configuration-directory configuration)
  ;; dynamic repositories - build location mirrors the source location
  ;; we will build kernel in $BINARY/$REPO/$BRANCH/kernel
  ;; where: $BINARY is destination in the configuration file
  ;; where: jazz:source is in $SOURCE/$REPO/$BRANCH
  (if (jazz:getf (jazz:get-configuration-properties configuration) dynamic?:)
      (jazz:build-dynamic-path (jazz:get-configuration-destination configuration) jazz:source)
    (jazz:destination-directory
      (jazz:get-configuration-name configuration)
      (jazz:get-configuration-destination configuration)
      "./"))))

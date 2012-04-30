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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(jazz:kernel-declares)


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
   (windowing           getter: generate)
   (safety              getter: generate)
   (optimize?           getter: generate)
   (debug-environments? getter: generate)
   (debug-location?     getter: generate)
   (debug-source?       getter: generate)
   (mutable-bindings?   getter: generate)
   (kernel-interpret?   getter: generate)
   (destination         getter: generate)
   (properties          getter: generate)))


(define (jazz:new-configuration
          #!key
          (name (jazz:unspecified-option))
          (system (jazz:unspecified-option))
          (platform (jazz:unspecified-option))
          (windowing (jazz:unspecified-option))
          (safety (jazz:unspecified-option))
          (optimize? (jazz:unspecified-option))
          (debug-environments? (jazz:unspecified-option))
          (debug-location? (jazz:unspecified-option))
          (debug-source? (jazz:unspecified-option))
          (mutable-bindings? (jazz:unspecified-option))
          (kernel-interpret? (jazz:unspecified-option))
          (destination (jazz:unspecified-option))
          (properties (jazz:unspecified-option)))
  (let* ((name (jazz:validate-name (jazz:require-name name)))
         (system (jazz:validate-system (jazz:require-system system)))
         (platform (jazz:validate-platform (jazz:require-platform platform)))
         (windowing (jazz:validate-windowing (jazz:require-windowing platform windowing)))
         (safety (jazz:validate-safety (jazz:require-safety safety)))
         (optimize? (jazz:validate-optimize? (jazz:require-optimize? optimize?)))
         (debug-environments? (jazz:validate-debug-environments? (jazz:require-debug-environments? debug-environments?)))
         (debug-location? (jazz:validate-debug-location? (jazz:require-debug-location? debug-location?)))
         (debug-source? (jazz:validate-debug-source? (jazz:require-debug-source? debug-source?)))
         (mutable-bindings? (jazz:validate-mutable-bindings? (jazz:require-mutable-bindings? mutable-bindings?)))
         (kernel-interpret? (jazz:validate-kernel-interpret? (jazz:require-kernel-interpret? kernel-interpret?)))
         (destination (jazz:validate-destination (jazz:require-destination destination)))
         (properties (jazz:validate-properties (jazz:require-properties properties))))
  (jazz:make-configuration
    name
    system
    platform
    windowing
    safety
    optimize?
    debug-environments?
    debug-location?
    debug-source?
    mutable-bindings?
    kernel-interpret?
    destination
    properties))))

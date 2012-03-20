;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Runtime
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
;;;    Stephane Le Cornec
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


(unit protected core.unit.runtime


(require (core.base))


(define (jazz:for-each-subunit toplevel-name proc)
  (let ((subunits (%%make-table test: eq?)))
    (let iter ((unit-name toplevel-name) (declaration (jazz:outline-unit toplevel-name)) (phase #f))
      (define (process-require require)
        (jazz:parse-require require
          (lambda (name feature-requirement phase)
            (iterate unit-name name phase))))
      
      (define (process-export export)
        (let ((reference (%%get-module-invoice-module export)))
          (if reference
              (let ((name (%%get-declaration-reference-name reference))
                    (phase (%%get-module-invoice-phase export)))
                (iterate unit-name name phase)))))
      
      (define (iterate parent-name unit-name phase)
        (let ((declaration (jazz:outline-unit unit-name)))
          (if (%%eq? (%%get-declaration-access declaration) 'protected)
              (if (%%not (jazz:descendant-unit? toplevel-name unit-name))
                  (jazz:error "Illegal access from {a} to protected unit {a}" toplevel-name unit-name)
                #; ;; debugging
                (let ((actual (%%table-ref subunits unit-name #f)))
                  (if actual
                      (jazz:debug 'duplicate 'export 'for unit-name '-> actual parent-name)))
                (begin
                  (%%table-set! subunits unit-name parent-name)
                  (iter unit-name declaration phase))))))
      
      (proc unit-name declaration phase)
      (if (jazz:is? declaration jazz:Unit-Declaration)
          (for-each process-require (%%get-unit-declaration-requires declaration))
        (begin
          (for-each process-require (%%get-module-declaration-requires declaration))
          (for-each process-export (%%get-module-declaration-exports declaration))))))))

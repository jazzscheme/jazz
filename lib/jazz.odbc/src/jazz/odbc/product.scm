;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; ODBC Product
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


(unit jazz.odbc.product


;;;
;;;; Build
;;;


(cond-expand
  (windows
    (define jazz:odbc-units
      `((jazz.odbc.odbc-lowlevel ld-options: "-lodbc32"))))
  (else
    (define jazz:odbc-units
      '((jazz.odbc.odbc-lowlevel))
      #; ;; some platforms are missing sql.h
      '((jazz.odbc.odbc-lowlevel ld-options: "-lodbc")))))


(define (jazz:build-odbc descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:odbc-units))
    (jazz:custom-compile/build unit-specs unit: unit force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.odbc
  build: jazz:build-odbc))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; OpenSSL Product
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


(unit jazz.openssl.product


;;;
;;;; Build
;;;


(cond-expand
  (silicon
    (define jazz:openssl-files
      (list (cons "foreign/jazz.openssl/silicon/lib/libcrypto.1.1.dylib" "Libraries/libcrypto.1.1.dylib")
            (cons "foreign/jazz.openssl/silicon/lib/libssl.1.1.dylib" "Libraries/libssl.1.1.dylib"))))
  (else
    (define jazz:openssl-files
      '())))


(define (jazz:copy-openssl-files)
  (let ((source jazz:kernel-source)
        (build (%%get-repository-directory jazz:Build-Repository)))
    (define (source-file path)
      (string-append source path))

    (define (build-file path)
      (string-append build path))

    (for-each (lambda (info)
                (let ((source (car info))
                      (build (cdr info)))
                  (jazz:copy&sign-if-needed (source-file source) (build-file build) feedback: jazz:feedback)))
              jazz:openssl-files)))


(define (jazz:build-openssl descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (let ((unit-specs '()))
    (jazz:custom-compile/build unit-specs pre-build: jazz:copy-openssl-files unit: unit force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor unit: unit force?: force?))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.openssl
  build: jazz:build-openssl))

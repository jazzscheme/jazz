;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Resources Product
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(unit jazz.resources


(define (jazz.build-resources descriptor #!key (unit #f) (force? #f))
  (let ((build (%%repository-directory jazz.Build-Repository))
        (source jazz.kernel-source))
    (define (build-file path)
      (string-append build path))
    
    (define (source-file path)
      (string-append source path))
    
    (define (copy-directory dir)
      (jazz.copy-directory (source-file dir) (build-file dir) feedback: jazz.feedback))
    
    (define (ensure-directory-exists dir)
      (if (not (file-exists? dir))
          (create-directory dir)))
    
    (define (copy-resource-directories)
      (ensure-directory-exists (build-file "lib"))
      (ensure-directory-exists (build-file "lib/jazz.resources"))
      (copy-directory "lib/jazz.resources/resources")
      (copy-directory "lib/jazz.resources/resources/cursors")
      (copy-directory "lib/jazz.resources/resources/images")
      (copy-directory "lib/jazz.resources/resources/sounds"))
    
    (jazz.custom-compile/build '() unit: unit pre-build: copy-resource-directories force?: force?)
    (jazz.update-product-descriptor descriptor)))


;;;
;;;; Register
;;;


(jazz.register-product 'jazz.resources
  build: jazz.build-resources))
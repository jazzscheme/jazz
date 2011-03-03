;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Vectors
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


(unit protected core.base.runtime.vector


(define (jazz:vector-for-each proc vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (begin
            (proc (%%vector-ref vector n))
            (iter (%%fx+ n 1)))))))


(define (jazz:vector-memq? obj vector)
  (let ((len (%%vector-length vector)))
    (let iter ((n 0))
      (if (%%fx< n len)
          (if (%%eq? (%%vector-ref vector n) obj)
              #t
            (iter (%%fx+ n 1)))
        #f))))


(define (jazz:resize-vector vector size)
  (let ((new-vector (%%make-vector size #f)))
    (let iter ((offset (%%fx- (min size (%%vector-length vector)) 1)))
      (%%when (%%fx>= offset 0)
        (%%vector-set! new-vector offset (%%vector-ref vector offset))
        (iter (%%fx- offset 1))))
    new-vector)))

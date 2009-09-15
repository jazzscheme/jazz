;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Templates
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


(library protected jazz.dialect.syntax.templates scheme


(import ;(jazz.dialect.syntax.template)
        )


(macro public (instantiate-for-each name T)
  `(specialize as ,name (for-each proc seq ,T)
     (let ((end (- (cardinality seq) 1)))
       (let (iterate (n 0))
         (when (<= n end)
           (proc (element seq n))
           (iterate (+ n 1)))))))


(macro public (instantiate-butlast T)
  `(specialize (butlast seq ,T) ,T
     (subseq seq 0 (- (cardinality seq) 1))))


;; #f should be {} when this is moved into a jazz dialect file
;; using <fx> is not 100% correct and should also be part of the template or better have smarter inferences
(macro public (instantiate-find name T)
  `(specialize as ,name (find seq ,T target (key: key #f) (test: test #f) (start: start #f) (end: end #f) (reversed?: reversed? #f)) <int+>
     (let ((len (cardinality seq))
           (test (or test eqv?))
           (inside (if (not reversed?) <= >=))
           (next (if (not reversed?) + -)))
       (let ((start <fx> (or start (if (not reversed?) 0 (- len 1))))
             (end <fx> (or end (if (not reversed?) (- len 1) 0))))
         (let (iterate (n start))
           (if (inside n end)
               (let ((obj (element seq n)))
                 (if (test (if key (key obj) obj) target)
                     n
                   (iterate (next n 1))))
             #f))))))


;; #f should be {} when this is moved into a jazz dialect file
;; using <fx> is not 100% correct and should also be part of the template or better have smarter inferences
(macro public (instantiate-find-in name T)
  `(specialize as ,name (find-in seq ,T target (key: key #f) (test: test #f) (start: start #f) (end: end #f) (reversed?: reversed? #f))
     (let ((len (cardinality seq))
           (test (or test eqv?))
           (inside (if (not reversed?) <= >=))
           (next (if (not reversed?) + -)))
       (let ((start <fx> (or start (if (not reversed?) 0 (- len 1))))
             (end <fx> (or end (if (not reversed?) (- len 1) 0))))
         (let (iterate (n start))
           (if (inside n end)
               (let ((obj (element seq n)))
                 (if (test (if key (key obj) obj) target)
                     obj
                   (iterate (next n 1))))
             #f))))))


(macro public (instantiate-starts-with? T)
  `(specialize (starts-with? seq ,T target ,T) <bool>
     (let ((slen (cardinality seq))
           (tlen (cardinality target)))
       (and (>= slen tlen)
            (= (subseq seq 0 tlen) target)))))


(macro public (instantiate-ends-with? T)
  `(specialize (ends-with? seq ,T target ,T) <bool>
     (let ((slen (cardinality seq))
           (tlen (cardinality target)))
       (and (>= slen tlen)
            (= (subseq seq (- slen tlen) slen) target))))))

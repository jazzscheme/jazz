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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2007
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


(library jazz.dialect.syntax.templates scheme


(import (jazz.dialect.kernel.boot)
        ;(jazz.dialect.syntax.template)
        )


(syntax (instantiate-for-each name T)
  `(specialize as ,name (for-each proc seq ,T)
     (let ((end (- (cardinality seq) 1)))
       (let (iterate (n 0))
         (when (<= n end)
           (proc (element seq n))
           (iterate (+ n 1)))))))


(syntax (instantiate-butlast T)
  `(specialize (butlast seq ,T) ,T
     (subseq seq 0 (- (cardinality seq) 1))))


;; #f should be {} when this is moved into a jazz dialect file
;; using <fx> is not 100% correct and should also be part of the template or better have smarter inferences
(syntax (instantiate-find name T)
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
(syntax (instantiate-find-in name T)
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


#; ;; wait
(syntax (instantiate-search name T)
  `(begin
     (definition (multisearch-impl seq <Sequence> contexts <list> start <fx+> reverse? <bool>)
       (let ((len (cardinality seq)))
         (let ((pos (either start (if reverse? len 0))))
           (let (iter (scan contexts))
             (if (null? scan)
                 #f
               (let ((context (car scan)))
                 (let ((target (get-target~ context))
                       (whole-words? (get-whole-words?~ context))
                       (ignore-case? (get-ignore-case?~ context))
                       (constituent-test (get-constituent-test~ context)))
                   (let ((test (if ignore-case? char-ci=? char=?))
                         (size (cardinality target)))
                     
                     (define (match-case? pos <fx>) <bool>
                       (let ((to (- size 1)))
                         (let (iter (i <fx> pos) (j <fx> 0))
                           (cond ((>= i len)
                                  #f)
                                 ((not (test (element seq i) (element target j)))
                                  #f)
                                 ((>= j to)
                                  #t)
                                 (else
                                  (iter (+ i 1) (+ j 1)))))))
                     
                     (define (match? pos <fx>) <bool>
                       (and (>= pos 0)
                            (match-case? pos)
                            (or (not whole-words?)
                                (let ((before (- pos 1))
                                      (after (+ pos size)))
                                  (if (and (>= before 0)
                                           (constituent-test (element seq before)))
                                      #f
                                    (if (< after len)
                                        (let ((c (element seq after)))
                                          ;; This explicit test for ~ is a kind of patch for the new ~ syntax
                                          (or (eqv? c #\~)
                                              (not (constituent-test (element seq after)))))
                                      #t))))))
                     
                     (define (find-backward) <fx+>
                       (let (iter (pos <fx> pos))
                         (cond ((match? (- pos size))
                                (- pos size))
                               ((<= pos 0)
                                {})
                               (else
                                (iter (- pos 1))))))
                     
                     (define (find-forward) <fx+>
                       (let ((last (- len size)))
                         (let (iter (pos <fx> pos))
                           (cond ((match? pos)
                                  pos)
                                 ((>= pos last)
                                  {})
                                 (else
                                  (iter (+ pos 1)))))))
                     
                     (let ((found ((if reverse? find-backward find-forward))))
                       (if found
                           (cons context found)
                         (iter (cdr scan))))))))))))
     
     (definition public (multisearch seq <Sequence> contexts <list>
                          (start: start {})
                          (reverse?: reverse? #f))
       (multisearch-impl seq contexts start reverse?))
     
     (definition public (multisearch-all seq <Sequence> contexts <list>
                          (start: start {})
                          (reverse?: reverse? #f))
       (let ((len (cardinality seq))
             (queue (new-queue)))
         (let (iter (pos <fx> (either start (if reverse? len 0))))
           (let ((found (multisearch-impl seq contexts pos reverse?)))
             (if found
                 (begin
                   (enqueue queue found)
                   (iter (if reverse?
                             (cdr found)
                           (+ (cdr found) (cardinality (get-target~ (car found)))))))
               (queue-list queue))))))
     
     (definition public (search seq <Sequence> target <Object>
                          (start: start {})
                          (reverse?: reverse? #f)
                          (whole-words?: whole-words? #f)
                          (ignore-case?: ignore-case? #f)
                          (constituent-test: constituent-test word-constituent?)) <int+>
       (let ((result (multisearch-impl seq (list (construct-search-context target whole-words? ignore-case? constituent-test #f)) start reverse?)))
         (if result
             (cdr result)
           #f)))
     
     (definition public (search-all seq <Sequence> target <Object>
                          (start: start {})
                          (reverse?: reverse? #f)
                          (whole-words?: whole-words? #f)
                          (ignore-case?: ignore-case? #f)
                          (constituent-test: constituent-test word-constituent?)) <list>
       (map cdr (multisearch-all seq (list (construct-search-context target whole-words? ignore-case? constituent-test #f))
                  start: start
                  reverse?: reverse?)))))


(syntax (instantiate-starts-with? T)
  `(specialize (starts-with? seq ,T target ,T) <bool>
     (let ((slen (cardinality seq))
           (tlen (cardinality target)))
       (and (>= slen tlen)
            (= (subseq seq 0 tlen) target)))))


(syntax (instantiate-ends-with? T)
  `(specialize (ends-with? seq ,T target ,T) <bool>
     (let ((slen (cardinality seq))
           (tlen (cardinality target)))
       (and (>= slen tlen)
            (= (subseq seq (- slen tlen) slen) target))))))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Advise
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


;;;
;;;; Advise
;;;


(define (jazz.advise symbol advice)
  (advice symbol
          (lambda ()
            (##global-var-ref symbol))
          (lambda (value)
            (##global-var-set! symbol value))))


;;;
;;;; Table
;;;


(define jazz.debugging-table-scan
  (make-parameter #f))


(define (jazz.debug-table-assert name table)
  (if (eq? (jazz.debugging-table-scan) table)
      (jazz.error "Calling prohibited {a} while scanning table: {a}" name table)))


(define (jazz.debug-table-find rest)
  (let loop ((rest rest))
       (cond ((null? rest)
              (jazz.error "Unable to find table in arguments"))
             ((table? (car rest))
              (car rest))
             (else
              (loop (cdr rest))))))


(define (jazz.debugging-table-advice symbol ref set)
  (let ((original (ref)))
    (set (lambda rest
           (let ((table (jazz.debug-table-find rest)))
             (jazz.debug-table-assert symbol table)
             (parameterize ((jazz.debugging-table-scan table))
               (apply original rest)))))))


(define (jazz.debug-table-advice symbol ref set)
  (let ((original (ref)))
    (set (lambda rest
           (let ((table (jazz.debug-table-find rest)))
             (jazz.debug-table-assert symbol table)
             (apply original rest))))))


(define (jazz.debug-table-scan)
  (jazz.advise 'table-for-each   jazz.debugging-table-advice)
  (jazz.advise '##table-for-each jazz.debugging-table-advice)
  (jazz.advise 'table-ref        jazz.debug-table-advice)
  (jazz.advise '##table-ref      jazz.debug-table-advice)
  (jazz.advise 'table-set!       jazz.debug-table-advice)
  (jazz.advise '##table-set!     jazz.debug-table-advice)
  (jazz.advise 'table-search     jazz.debug-table-advice)
  (jazz.advise '##table-search   jazz.debug-table-advice)
  (jazz.advise 'table-copy       jazz.debug-table-advice)
  (jazz.advise '##table-copy     jazz.debug-table-advice)
  (jazz.advise 'table-merge      jazz.debug-table-advice)
  (jazz.advise '##table-merge    jazz.debug-table-advice)
  (jazz.advise 'table-merge!     jazz.debug-table-advice)
  (jazz.advise '##table-merge!   jazz.debug-table-advice)
  (jazz.advise 'table->list      jazz.debug-table-advice)
  (jazz.advise '##table->list    jazz.debug-table-advice))

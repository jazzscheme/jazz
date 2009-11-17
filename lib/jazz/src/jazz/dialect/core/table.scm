;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Tables
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


(unit protected jazz.dialect.core.table


(define (jazz.table-clear table key)
  (%%debug-assert (%%table? table)
    (%%table-clear table key)))


(define (jazz.table-keys table)
  (%%debug-assert (%%table? table)
    (%%table-keys table)))


(define (jazz.table-length table)
  (%%debug-assert (%%table? table)
    (%%table-length table)))


(define (jazz.iterate-table table proc)
  (%%debug-assert (%%table? table)
    (%%iterate-table table proc)))


(define (jazz.map-table table proc)
  (%%debug-assert (%%table? table)
    (let ((queue (jazz.new-queue)))
      (%%iterate-table table
        (lambda (key value)
          (jazz.enqueue queue (proc key value))))
      (jazz.queue-list queue))))


(define (jazz.list->table alist #!key (test equal?))
  (%%list->table alist test: test))


(define (jazz.table->list table)
  (%%debug-assert (%%table? table)
    (%%table->list table)))


(define (jazz.table-entries table)
  (%%debug-assert (%%table? table)
    (%%table-entries table)))


(cond-expand
  (gambit
    (define jazz.eq?-hash eq?-hash)
    (define jazz.eqv?-hash eqv?-hash)
    (define jazz.equal?-hash equal?-hash))
  (else)))

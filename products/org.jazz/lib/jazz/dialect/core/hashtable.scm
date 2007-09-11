;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Module Runtime
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2006
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


(module jazz.dialect.core.hashtable


(define (jazz.new-hashtable #!optional (test equal?))
  (%%new-hashtable test))


(define (jazz.hashtable? obj)
  (%%hashtable? obj))


(define (jazz.hashtable-ref hashtable key #!optional (default #f))
  (%%assert (%%hashtable? hashtable)
    (%%hashtable-ref hashtable key default)))


(define (jazz.hashtable-set! hashtable key value)
  (%%assert (%%hashtable? hashtable)
    (%%hashtable-set! hashtable key value)))


(define (jazz.hashtable-clear hashtable key)
  (%%assert (%%hashtable? hashtable)
    (%%hashtable-clear hashtable key)))


(define (jazz.hashtable-keys hashtable)
  (%%assert (%%hashtable? hashtable)
    (%%hashtable-keys hashtable)))


(define (jazz.iterate-hashtable hashtable proc)
  (%%assert (%%hashtable? hashtable)
    (%%iterate-hashtable hashtable proc)))


(define (jazz.alist->hashtable alist #!optional (test equal?))
  (%%alist->hashtable alist test))


(define (jazz.hashtable->alist hashtable)
  (%%assert (%%hashtable? hashtable)
    (%%hashtable->alist hashtable)))


(define (jazz.hashtable-entries hashtable)
  (%%assert (%%hashtable? hashtable)
    (%%hashtable-entries hashtable))))

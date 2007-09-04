;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Hashtable Primitives
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


(cond-expand
  (chicken
    (define-macro (%%hashtable-name->associator test-name)
      `(case ,test-name
         ((:eq?) assq)
         ((:eqv?) assv)
         ((:equal?) assoc)))
      
    (define-macro (%%new-hashtable test-name)
      `(make-hash-table (%%hashtable-name->associator ,test-name)))
    
    (define-macro (%%hashtable-ref hashtable key default)
      `(hash-table-ref/default ,hashtable ,key ,default))
    
    (define-macro (%%hashtable-set! hashtable key value)
      `(hash-table-set! ,hashtable ,key ,value))
    
    (define-macro (%%hashtable-keys hashtable)
      `(hash-table-keys ,hashtable))
    
    (define-macro (%%iterate-hashtable hashtable proc)
      `(hash-table-walk ,hashtable ,proc))
    
    (define-macro (%%alist->hashtable alist test-name)
      `(alist->hash-table ,alist)))
  
  (gambit
    (define-macro (%%hashtable? obj)
      `(table? ,obj))
    
    (define-macro (%%hashtable-name->test test-name)
      `(case ,test-name
         ((:eq?) eq?)
         ((:eqv?) eqv?)
         ((:equal?) equal?)))
    
    (define-macro (%%new-hashtable test-name)
      `(make-table test: (%%hashtable-name->test ,test-name)))
    
    (define-macro (%%hashtable-ref hashtable key default)
      (if (jazz.safe?)
          `(table-ref ,hashtable ,key ,default)
        `(##table-ref ,hashtable ,key ,default)))
    
    (define-macro (%%hashtable-set! hashtable key value)
      (if (jazz.safe?)
          `(table-set! ,hashtable ,key ,value)
        `(##table-set! ,hashtable ,key ,value)))
    
    (define-macro (%%hashtable-clear hashtable key)
      `(table-set! ,hashtable ,key))
    
    (define-macro (%%hashtable-keys hashtable)
      `(map car (table->list ,hashtable)))
    
    (define-macro (%%iterate-hashtable hashtable proc)
      `(table-for-each ,proc ,hashtable))
    
    (define-macro (%%alist->hashtable alist test-name)
      `(list->table ,alist test: (%%hashtable-name->test ,test-name)))
    
    (define-macro (%%hashtable->alist hashtable)
      `(table->list ,hashtable))
    
    (define-macro (%%hashtable-entries hashtable)
      `(table-length ,hashtable)))
  
  (else))

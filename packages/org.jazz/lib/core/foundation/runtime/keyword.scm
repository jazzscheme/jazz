;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Keywords
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


(module core.foundation.runtime.keyword


(cond-expand
  (chicken
    (define (jazz.keyword? obj)
      (keyword? obj))
    
    (define (jazz.keyword->string keyword)
      (keyword->string keyword))
    
    (define (jazz.string->keyword string)
      (string->keyword string)))
  
  (gambit
    (define (jazz.keyword? obj)
      (keyword? obj))
    
    (define (jazz.keyword->string keyword)
      (keyword->string keyword))
    
    (define (jazz.string->keyword string)
      (string->keyword string)))
  
  (else
   (define (jazz.keyword? obj)
     (and (%%symbol? obj)
          (let ((str (%%symbol->string obj)))
            (%%eqv? (%%string-ref str (%%fixnum- (%%string-length str) 1)) #\:))))
   
   
   (define (jazz.keyword->string keyword)
     (let ((str (%%symbol->string keyword)))
       (%%substring str 0 (%%fixnum- (%%length str) 1))))
   
   
   (define (jazz.string->keyword string)
     (%%string->symbol (%%string-append string ":"))))))

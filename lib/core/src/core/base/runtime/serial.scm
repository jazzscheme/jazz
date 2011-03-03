;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Serial Numbers
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


(unit protected core.base.runtime.serial


(cond-expand
  (gambit
    (define (jazz:object->serial obj)
      (object->serial-number obj))
    
    (define (jazz:serial->object number)
      (serial-number->object number))
    
    ;; for debugging
    (define (jazz:object->serial-symbol obj)
      (%%string->symbol (%%string-append "#" (%%number->string (jazz:object->serial obj))))))
  
  (else
   ;; Incorrect implementation that will not let the serialized objects be
   ;; garbage collected. Weak tables are needed for a correct implementation...
   
   (define jazz:serial-number
     1)
   
   (define jazz:serialized-objects
     (%%make-table test: equal?))
   
   (define (jazz:object->serial obj)
     (or (%%table-ref jazz:serialized-objects obj #f)
         (let ((number jazz:serial-number))
           (set! jazz:serial-number (%%fx+ jazz:serial-number 1))
           (%%table-set! jazz:serialized-objects obj number)
           number)))
   
   (define (jazz:serial->object number . rest)
     (continuation-capture
       (lambda (return)
         (jazz:iterate-table jazz:serialized-objects
           (lambda (key value)
             (if (%%fx= value number)
                 (continuation-return return key))))
         (if (%%null? rest)
             (jazz:error "Unbound serial number: {s}" number)
           (%%car rest))))))))

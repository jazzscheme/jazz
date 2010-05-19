;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Macro Expansion
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


(block kernel.expansion


(define (jazz.generate-symbol #!optional (prefix "sym"))
  (let ((for (jazz.generate-symbol-for))
        (context (jazz.generate-symbol-context))
        (counter (jazz.generate-symbol-counter)))
    (if (not context)
        (error "Invalid call to generate-symbol without a context")
      (let ((module (jazz.string-replace (##symbol->string context) #\. #\/)))
        (let ((name (##string-append module "_" prefix (or for "^") (##number->string counter))))
          (if (##find-interned-symbol name)
              (error "Detected invalid state:" name)
            (begin
              (jazz.generate-symbol-counter (+ counter 1))
              (##string->symbol name))))))))


(define (jazz.generate-lexical-symbol #!optional (prefix "sym"))
  (let ((for (jazz.generate-symbol-for))
        (counter (jazz.generate-symbol-counter)))
    (if (not counter)
        (error "Invalid call to generate-symbol without a counter")
      (let ((name (##string-append prefix (or for "^") (##number->string counter))))
        (if (##find-interned-symbol name)
            (error "Detected invalid state:" name)
          (begin
            (jazz.generate-symbol-counter (+ counter 1))
            (##string->symbol name)))))))


(define (jazz.simplify-begin form)
  (if (and (##pair? form)
           (##eq? (##car form) 'begin)
           (##pair? (##cdr form))
           (##null? (##cddr form)))
      (##cadr form)
    form))


(define (jazz.with-uniqueness expr proc)
  (if (##symbol? (jazz.source-code expr))
      (proc expr)
    (let ((value (jazz.generate-symbol "val")))
      `(let ((,value ,expr))
         ,(proc value)))))


(jazz.define-macro (%%force-uniqueness variables code)
  (if (##null? variables)
      code
    (let ((variable (##car variables)))
      `(jazz.with-uniqueness ,variable
         (lambda (,variable)
           (%%force-uniqueness ,(##cdr variables) ,code))))))


;;;
;;;; Check
;;;


(jazz.define-macro (jazz.define-check-macro name test type)
  `(jazz.define-macro (,name arg pos call code)
     (if jazz.debug-core?
         `(if (,',test ,arg)
              ,code
            (jazz.primitive-type-error ,pos ,',type ',(##car call) (##list ,@(##cdr call))))
       code)))


(jazz.define-check-macro %%check-continuation
  ##continuation?
  "CONTINUATION")

(jazz.define-check-macro %%check-fixnum
  ##fixnum?
  "FIXNUM")

(jazz.define-check-macro %%check-foreign
  ##foreign?
  "FOREIGN")

(jazz.define-check-macro %%check-list
  list?
  "LIST")

(jazz.define-check-macro %%check-locat
  ##locat?
  "LOCAT")

(jazz.define-check-macro %%check-port
  ##port?
  "PORT")

(jazz.define-check-macro %%check-procedure
  ##procedure?
  "PROCEDURE")

(jazz.define-check-macro %%check-readenv
  jazz.readenv?
  "READENV")

(jazz.define-check-macro %%check-readtable
  ##readtable?
  "READTABLE")

(jazz.define-check-macro %%check-source
  ##source?
  "SOURCE")

(jazz.define-check-macro %%check-string
  ##string?
  "STRING")

(jazz.define-check-macro %%check-symbol
  ##symbol?
  "SYMBOL"))

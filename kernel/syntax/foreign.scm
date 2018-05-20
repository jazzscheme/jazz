;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Foreign Expansion
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2018
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


(block kernel.foreign


;; quicky duplicated code
(define enter-marker
  ";---> ")

(define exit-marker
  ";<--- ")


(jazz:define-macro (c-include include)
  (let ((str (string-append "#include " include)))
    (if (%%not jazz:kernel-debug-foreign?)
        `(c-declare ,str)
      `(begin
         (jazz:logging-line ,(string-append enter-marker str))
         (c-declare ,str)
         (jazz:logging-line ,(string-append exit-marker str))))))


(jazz:define-macro (c-declaration name-or-declaration . rest)
  (let ((name (if (symbol? name-or-declaration) name-or-declaration 'anonymous))
        (declaration (if (symbol? name-or-declaration) (car rest) name-or-declaration)))
    (if (%%not jazz:kernel-debug-foreign?)
        `(c-declare ,declaration)
      (let ((str (symbol->string name)))
        `(begin
           (jazz:logging-line ,(string-append enter-marker str " declaration"))
           (c-declare ,declaration)
           (jazz:logging-line ,(string-append exit-marker str " declaration")))))))


(jazz:define-macro (c-initialization name-or-code . rest)
  (let ((name (if (symbol? name-or-code) name-or-code 'anonymous))
        (code (if (symbol? name-or-code) (car rest) name-or-code)))
    (if (%%not jazz:kernel-debug-foreign?)
        `(c-initialize ,code)
      (let ((str (symbol->string name)))
        `(begin
           (jazz:logging-line ,(string-append enter-marker str " initialization"))
           (c-initialize ,code)
           (jazz:logging-line ,(string-append exit-marker str " initialization")))))))


(jazz:define-macro (c-definition signature types result-type c-name scope . body)
  (if (%%not jazz:kernel-debug-foreign?)
      `(c-define ,signature ,types ,result-type ,c-name ,scope
         ,@body)
    (let ((name (car signature)))
      `(c-define ,signature ,types ,result-type ,c-name ,scope
         (jazz:logging-c->scheme ',name
           (lambda ()
             ,@body))))))


(jazz:define-macro (c-function name types result-type c-name-or-code)
  (if (%%not jazz:kernel-debug-foreign?)
      `(c-lambda ,types ,result-type ,c-name-or-code)
    (let ((variables (map (lambda (type)
                            (jazz:generate-symbol))
                          types)))
      `(lambda ,variables
         (jazz:logging-scheme->c ',name
           (lambda ()
             ((c-lambda ,types ,result-type ,c-name-or-code)
              ,@variables))
           ,@variables)))))


(jazz:define-macro (c-external signature result-type . rest)
  (let ((name (car signature))
        (types (cdr signature)))
    (let ((c-name-or-code (if (null? rest) (symbol->string name) (car rest))))
      (if (%%not jazz:kernel-debug-foreign?)
          `(define ,name
             (c-lambda ,types ,result-type ,c-name-or-code))
        (let ((variables (map (lambda (type)
                                (jazz:generate-symbol))
                              types)))
          `(define ,name
             (lambda ,variables
               (jazz:logging-scheme->c ',name
                 (lambda ()
                   ((c-lambda ,types ,result-type ,c-name-or-code)
                    ,@variables))
                 ,@variables))))))))


#; ;; quicky as this cannot refer to c-function???
(jazz:define-macro (c-external signature result-type . rest)
  (let ((name (car signature))
        (types (cdr signature)))
    (let ((c-name-or-code (if (null? rest) (symbol->string name) (car rest))))
      `(define ,name
         (c-function ,types ,result-type ,c-name-or-code))))))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Symbols
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


(module core.base.runtime.symbol


;;;
;;;; Identifier
;;;


(define (jazz.identifier-name identifier)
  (%%assert (%%symbol? identifier)
    (let* ((str (%%symbol->string identifier))
           (pos (jazz.string-find-reversed str #\.)))
      (if (%%not pos)
          identifier
        (%%string->symbol (%%substring str (%%fx+ pos 1) (%%string-length str)))))))


;;;
;;;; Specifier
;;;


(define (jazz.specifier? expr)
  (and (%%symbol? expr)
       (let ((str (%%symbol->string expr)))
         (let ((len (%%string-length str)))
           (and (%%fx> len 2)
                (%%eqv? (%%string-ref str 0) #\<)
                (%%eqv? (%%string-ref str (%%fx- len 1)) #\>))))))


(define (jazz.specifier->name specifier)
  (let ((extract
          (lambda (string)
            (%%substring string 1 (%%fx- (%%string-length string) 1)))))
    (%%string->symbol (extract (%%symbol->string specifier)))))


(define (jazz.name->specifier name)
  (%%string->symbol (%%string-append "<" (%%symbol->string name) ">")))


(define (jazz.binding-specifier binding)
  (let ((cdr-binding (%%cdr (%%source-code binding))))
    (and (%%pair? cdr-binding)
         (jazz.specifier? (%%source-code (%%car cdr-binding)))
         (%%source-code (%%car cdr-binding)))))


;;;
;;;; Enumerator
;;;


(define (jazz.enumerator? obj)
  (and (%%symbol? obj)
       (%%eqv? (%%string-ref (%%symbol->string obj) 0) #\:)))


(define (jazz.enumerator->symbol obj)
  (let ((name (%%symbol->string obj)))
    (%%string->symbol (%%substring name 1 (%%string-length name))))))

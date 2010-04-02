;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; FFI
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


(unit protected core.module.syntax.walker.ffi


;;;
;;;; C Type
;;;


(jazz.define-class-runtime jazz.C-Type-Declaration)


(define (jazz.new-c-type-declaration name type access compatibility attributes parent kind expansion base-type inclusions c-to-scheme scheme-to-c declare)
  (let ((new-declaration (jazz.allocate-c-type-declaration jazz.C-Type-Declaration name type #f access compatibility attributes #f parent #f #f kind expansion base-type '() inclusions c-to-scheme scheme-to-c declare)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.get-declaration-inclusions (jazz.C-Type-Declaration declaration))
  (%%get-c-type-declaration-inclusions declaration))


(jazz.define-method (jazz.emit-declaration (jazz.C-Type-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.C-Type-Declaration declaration))
  (let ((locator (%%get-declaration-locator declaration))
        (expansion (%%get-c-type-declaration-expansion declaration))
        (c-to-scheme (%%get-c-type-declaration-c-to-scheme declaration))
        (scheme-to-c (%%get-c-type-declaration-scheme-to-c declaration)))
    `(c-define-type ,locator ,expansion ,@(if (and c-to-scheme scheme-to-c)
                                              (%%list c-to-scheme scheme-to-c #f)
                                            '()))))


(jazz.define-method (jazz.fold-declaration (jazz.C-Type-Declaration expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.C-Type-Declaration)


;;;
;;;; C Definition
;;;


(jazz.define-class-runtime jazz.C-Definition-Declaration)


(define (jazz.new-c-definition-declaration name type access compatibility attributes parent signature parameter-types result-type c-name scope)
  (let ((new-declaration (jazz.allocate-c-definition-declaration jazz.C-Definition-Declaration name type #f access compatibility attributes #f parent #f #f signature parameter-types result-type c-name scope #f)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.walk-binding-validate-call (jazz.C-Definition-Declaration declaration) walker resume source-declaration operator arguments form-src)
  (let ((signature (%%get-c-definition-declaration-signature declaration)))
    (if signature
        (jazz.validate-arguments walker resume source-declaration declaration signature arguments form-src))))


(jazz.define-method (jazz.emit-declaration (jazz.C-Definition-Declaration declaration) environment)
  (let ((locator (%%get-declaration-locator declaration))
        (signature (%%get-c-definition-declaration-signature declaration))
        (parameter-types (%%get-c-definition-declaration-parameter-types declaration))
        (result-type (%%get-c-definition-declaration-result-type declaration))
        (c-name (%%get-c-definition-declaration-c-name declaration))
        (scope (%%get-c-definition-declaration-scope declaration))
        (body (%%get-c-definition-declaration-body declaration)))
    (jazz.with-annotated-frame (jazz.annotate-signature signature)
      (lambda (frame)
        (let ((augmented-environment (%%cons frame environment)))
          (jazz.sourcify-if
            `(c-define ,(%%cons locator (jazz.emit-signature signature declaration augmented-environment)) ,parameter-types ,result-type ,c-name ,scope
               ,@(jazz.sourcified-form (jazz.emit-expression body declaration augmented-environment)))
            (%%get-declaration-source declaration)))))))


(jazz.define-method (jazz.emit-binding-reference (jazz.C-Definition-Declaration declaration) source-declaration environment)
  (jazz.new-code
    (%%get-declaration-locator declaration)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-declaration (jazz.C-Definition-Declaration declaration) f k s)
  (f declaration
     (k (jazz.fold-statement (%%get-c-definition-declaration-body declaration) f k s)
        s)))


(jazz.encapsulate-class jazz.C-Definition-Declaration)


;;;
;;;; C Include
;;;


(jazz.define-class-runtime jazz.C-Include)


(define (jazz.new-c-include name)
  (jazz.allocate-c-include jazz.C-Include #f #f name))


(jazz.define-method (jazz.emit-expression (jazz.C-Include expression) declaration environment)
  (let ((name (%%get-c-include-name expression)))
    (jazz.new-code
      `(c-declare ,(%%string-append "#include " name))
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.C-Include expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.C-Include)


;;;
;;;; C Declare
;;;


(jazz.define-class-runtime jazz.C-Declare)


(define (jazz.new-c-declare code)
  (jazz.allocate-c-declare jazz.C-Declare #f #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Declare expression) declaration environment)
  (let ((code (%%get-c-declare-code expression)))
    (jazz.new-code
      `(c-declare ,code)
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.C-Declare expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.C-Declare)


;;;
;;;; C Named Declare
;;;


(jazz.define-class-runtime jazz.C-Named-Declare-Declaration)


(define (jazz.new-c-named-declare-declaration name type access compatibility attributes parent code)
  (let ((new-declaration (jazz.allocate-c-named-declare-declaration jazz.C-Named-Declare-Declaration name type #f access compatibility attributes #f parent #f #f code)))
    (jazz.setup-declaration new-declaration)
    new-declaration))


(jazz.define-method (jazz.emit-declaration (jazz.C-Named-Declare-Declaration declaration) environment)
  `(begin))


(jazz.define-method (jazz.expand-referenced-declaration (jazz.C-Named-Declare-Declaration declaration))
  (let ((code (%%get-c-named-declare-declaration-code declaration)))
    `(c-declare ,code)))


(jazz.define-method (jazz.fold-declaration (jazz.C-Named-Declare-Declaration expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.C-Named-Declare-Declaration)


;;;
;;;; C Initialize
;;;


(jazz.define-class-runtime jazz.C-Initialize)


(define (jazz.new-c-initialize code)
  (jazz.allocate-c-initialize jazz.C-Initialize #f #f code))


(jazz.define-method (jazz.emit-expression (jazz.C-Initialize expression) declaration environment)
  (let ((code (%%get-c-initialize-code expression)))
    (jazz.new-code
      `(c-initialize ,code)
      jazz.Any
      #f)))


(jazz.define-method (jazz.fold-expression (jazz.C-Initialize expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.C-Initialize)


;;;
;;;; C Function
;;;


(jazz.define-class-runtime jazz.C-Function)


(define (jazz.new-c-function expansion)
  (jazz.allocate-c-function jazz.C-Function #f #f expansion))


(jazz.define-method (jazz.emit-expression (jazz.C-Function expression) declaration environment)
  (jazz.new-code
    (%%get-c-function-expansion expression)
    jazz.Any
    #f))


(jazz.define-method (jazz.fold-expression (jazz.C-Function expression) f k s)
  (f expression s))


(jazz.encapsulate-class jazz.C-Function))

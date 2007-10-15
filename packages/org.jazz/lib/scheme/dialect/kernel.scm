;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Scheme Kernel
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


(library scheme.dialect.kernel core


;;;
;;;; 4.2 Derived expression types
;;;


;;;
;;;; 4.2.1 Conditionals
;;;


;;;
;;;; 6.1 Equivalence predicates
;;;


(native eq? <bool>)
(native eqv? <bool>)
(native equal? <bool>)


;;;
;;;; 6.2 Numbers
;;;


;;;
;;;; 6.2.5 Numerical operations
;;;


(native number? <bool>)
(native complex? <bool>)
(native real? <bool>)
(native rational? <bool>)
(native integer? <bool>)
(native exact? <bool>)
(native inexact? <bool>)
(native = <bool>)
(native < <bool>)
(native > <bool>)
(native <= <bool>)
(native >= <bool>)
(native zero? <bool>)
(native positive? <bool>)
(native negative? <bool>)
(native odd? <bool>)
(native even? <bool>)
(native max <number>)
(native min <number>)
(native + <number>)
(native * <number>)
(native - <number>)
(native / <number>)
(native abs <number>)
(native quotient <number>)
(native remainder <number>)
(native modulo <number>)
(native gcd <number>)
(native lcm <number>)
(native numerator <number>)
(native denominator <number>)
(native floor <number>)
(native ceiling <number>)
(native truncate <number>)
(native round <number>)
(native rationalize <number>)
(native exp <number>)
(native log <number>)
(native sin <number>)
(native cos <number>)
(native tan <number>)
(native asin <number>)
(native acos <number>)
(native atan <number>)
(native sqrt <number>)
(native expt <number>)
(native make-rectangular <number>)
(native make-polar <number>)
(native real-part <number>)
(native imag-part <number>)
(native magnitude <number>)
(native angle <number>)
(native exact->inexact <number>)
(native inexact->exact <number>)


;;;
;;;; 6.2.6 Numerical input and output
;;;


(native number->string <string>)
(native string->number <number>)


;;;
;;;; 6.3 Other data types
;;;


;;;
;;;; 6.3.1 Booleans
;;;


(native not <bool>)
(native boolean? <bool>)


;;;
;;;; 6.3.2 Pairs and lists
;;;


(native pair? <bool>)
(native cons <pair>)
(native car <any>)
(native cdr <any>)
(native set-car! <void>)
(native set-cdr! <void>)
(native caar <any>)
(native cadr <any>)
(native cdar <any>)
(native cddr <any>)
(native caaar <any>)
(native caadr <any>)
(native cadar <any>)
(native caddr <any>)
(native cdaar <any>)
(native cdadr <any>)
(native cddar <any>)
(native cdddr <any>)
(native caaaar <any>)
(native caaadr <any>)
(native caadar <any>)
(native caaddr <any>)
(native cadaar <any>)
(native cadadr <any>)
(native caddar <any>)
(native cadddr <any>)
(native cdaaar <any>)
(native cdaadr <any>)
(native cdadar <any>)
(native cdaddr <any>)
(native cddaar <any>)
(native cddadr <any>)
(native cdddar <any>)
(native cddddr <any>)
(native null? <bool>)
(native list? <bool>)
(native list <list>)
(native length <int>)
(native append <list>)
(native reverse <list>)
(native list-tail <list>)
(native list-ref <any>)
(native memq <list+>)
(native memv <list+>)
(native member <list+>)
(native assq <pair+>)
(native assv <pair+>)
(native assoc <pair+>)


;;;
;;;; 6.3.3 Symbols
;;;


(native symbol? <bool>)
(native symbol->string <string>)
(native string->symbol <symbol>)


;;;
;;;; 6.3.4 Characters
;;;


(native char? <bool>)
(native char=? <bool>)
(native char<? <bool>)
(native char>? <bool>)
(native char<=? <bool>)
(native char>=? <bool>)
(native char-ci=? <bool>)
(native char-ci<? <bool>)
(native char-ci>? <bool>)
(native char-ci<=? <bool>)
(native char-ci>=? <bool>)
(native char-alphabetic? <bool>)
(native char-numeric? <bool>)
(native char-whitespace? <bool>)
(native char-upper-case? <bool>)
(native char-lower-case? <bool>)
(native char->integer <int>)
(native integer->char <char>)
(native char-upcase <char>)
(native char-downcase <char>)


;;;
;;;; 6.3.5 Strings
;;;


(native string? <bool>)
(native make-string <string>)
(native string <string>)
(native string-length <int>)
(native string-ref <char>)
(native string-set! <void>)
(native string=? <bool>)
(native string-ci=? <bool>)
(native string<? <bool>)
(native string>? <bool>)
(native string<=? <bool>)
(native string>=? <bool>)
(native string-ci<? <bool>)
(native string-ci>? <bool>)
(native string-ci<=? <bool>)
(native string-ci>=? <bool>)
(native substring <string>)
(native string-append <string>)
(native string->list <list>)
(native list->string <string>)
(native string-copy <string>)
(native string-fill! <void>)


;;;
;;;; 6.3.6 Vectors
;;;


(native vector? <bool>)
(native make-vector <vector>)
(native vector <vector>)
(native vector-length <int>)
(native vector-ref <any>)
(native vector-set! <void>)
(native vector->list <list>)
(native list->vector <vector>)
(native vector-fill! <void>)


;;;
;;;; 6.4 Control features
;;;


(native procedure? <bool>)
(native apply <any>)
(native map <list>)
(native for-each <void>)
(native force <any>)
(native call-with-current-continuation <any>)
(native call/cc <any>)
(native values <any>)
(native call-with-values <any>)
(native dynamic-wind <any>)


;;;
;;;; 6.5 Eval
;;;


(native eval <any>)
(native scheme-replace-report-environment <any>)
(native null-environment <any>)
(native interaction-environment <any>)


;;;
;;;; 6.6 Input and output
;;;


;;;
;;;; 6.6.1 Ports
;;;


(native call-with-input-file <any>)
(native call-with-output-file <any>)
(native input-port? <bool>)
(native output-port? <bool>)
(native current-input-port <port>)
(native current-output-port <port>)
(native with-input-from-file <any>)
(native with-output-to-file <any>)
(native open-input-file <port>)
(native open-output-file <port>)
(native close-input-port <void>)
(native close-output-port <void>)


;;;
;;;; 6.6.2 Input
;;;


(native read <any>)
(native read-char <any>)
(native peek-char <any>)
(native eof-object? <bool>)
(native char-ready? <bool>)


;;;
;;;; 6.6.3 Output
;;;


(native write <void>)
(native display <void>)
(native newline <void>)
(native write-char <void>)
(native load <void>)
(native transcript-on <void>)
(native transcript-off <void>))

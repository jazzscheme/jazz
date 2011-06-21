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


(module protected scheme.dialect.kernel core


;;;
;;;; 4.2 Derived expression types
;;;


;;;
;;;; 4.2.1 Conditionals
;;;


;;;
;;;; 6.1 Equivalence predicates
;;;


(native eq? <object^object:bool>)
(native eqv? <object^object:bool>)
(native equal? <object^object:bool>)


;;;
;;;; 6.2 Numbers
;;;


;;;
;;;; 6.2.5 Numerical operations
;;;


(native number? <object:bool>)
(native complex? <object:bool>)
(native real? <object:bool>)
(native rational? <object:bool>)
(native integer? <object:bool>)
(native exact? <object:bool>)
(native inexact? <object:bool>)
(native = <number*:bool>)
(native < <number*:bool>)
(native > <number*:bool>)
(native <= <number*:bool>)
(native >= <number*:bool>)
(native zero? <object:bool>)
(native positive? <object:bool>)
(native negative? <object:bool>)
(native odd? <object:bool>)
(native even? <object:bool>)
(native max <number*:number>)
(native min <number*:number>)
(native + <number*:number>)
(native * <number*:number>)
(native - <number*:number>)
(native / <number*:number>)
(native abs <number:number>)
(native quotient <number^number:number>)
(native remainder <number^number:number>)
(native modulo <number^number:number>)
(native gcd <number*:number>)
(native lcm <number*:number>)
(native numerator <rational:number>)
(native denominator <rational:number>)
(native floor <real:real>)
(native ceiling <real:real>)
(native truncate <real:real>)
(native round <real:real>)
(native rationalize <number^number:number>)
(native exp <number:number>)
(native log <number:number>)
(native sin <number:fl>)
(native cos <number:fl>)
(native tan <number:number>)
(native asin <number:number>)
(native acos <number:number>)
(native atan <number^opt<number>:number>)
(native sqrt <number:number>)
(native expt <number^number:number>)
(native make-rectangular <number^number:number>)
(native make-polar <number^number:number>)
(native real-part <number:number>)
(native imag-part <number:number>)
(native magnitude <number:number>)
(native angle <number:number>)
(native exact->inexact <number:number>)
(native inexact->exact <number:number>)


;;;
;;;; 6.2.6 Numerical input and output
;;;


(native number->string <number:string>)
(native string->number <string:number>)


;;;
;;;; 6.3 Other data types
;;;


;;;
;;;; 6.3.1 Booleans
;;;


(native not <object:bool>)
(native boolean? <object:bool>)


;;;
;;;; 6.3.2 Pairs and lists
;;;


(native pair? <object:bool>)
(native cons <object^object:pair>)
(native car <pair:object>)
(native cdr <pair:object>)
(native set-car! <pair:void>)
(native set-cdr! <pair:void>)
(native caar <pair:object>)
(native cadr <pair:object>)
(native cdar <pair:object>)
(native cddr <pair:object>)
(native caaar <pair:object>)
(native caadr <pair:object>)
(native cadar <pair:object>)
(native caddr <pair:object>)
(native cdaar <pair:object>)
(native cdadr <pair:object>)
(native cddar <pair:object>)
(native cdddr <pair:object>)
(native caaaar <pair:object>)
(native caaadr <pair:object>)
(native caadar <pair:object>)
(native caaddr <pair:object>)
(native cadaar <pair:object>)
(native cadadr <pair:object>)
(native caddar <pair:object>)
(native cadddr <pair:object>)
(native cdaaar <pair:object>)
(native cdaadr <pair:object>)
(native cdadar <pair:object>)
(native cdaddr <pair:object>)
(native cddaar <pair:object>)
(native cddadr <pair:object>)
(native cdddar <pair:object>)
(native cddddr <pair:object>)
(native null? <object:bool>)
(native list? <object:bool>)
(native list <object*:list>)
(native length <list:int>)
(native append <list*:list>)
(native reverse <list:list>)
(native list-tail <list:list>)
(native list-ref <list:object>)
(native memq <object^list:list+>)
(native memv <object^list:list+>)
(native member <object^list:list+>)
(native assq <object^list:pair+>)
(native assv <object^list:pair+>)
(native assoc <object^list:pair+>)


;;;
;;;; 6.3.3 Symbols
;;;


(native symbol? <object:bool>)
(native symbol->string <symbol:string>)
(native string->symbol <string:symbol>)


;;;
;;;; 6.3.4 Characters
;;;


(native char? <object:bool>)
(native char=? <char^char:bool>)
(native char<? <char^char:bool>)
(native char>? <char^char:bool>)
(native char<=? <char^char:bool>)
(native char>=? <char^char:bool>)
(native char-ci=? <char^char:bool>)
(native char-ci<? <char^char:bool>)
(native char-ci>? <char^char:bool>)
(native char-ci<=? <char^char:bool>)
(native char-ci>=? <char^char:bool>)
(native char-alphabetic? <char:bool>)
(native char-numeric? <char:bool>)
(native char-whitespace? <char:bool>)
(native char-upper-case? <char:bool>)
(native char-lower-case? <char:bool>)
(native char->integer <char:fx>)
(native integer->char <fx:char>)
(native char-upcase <char:char>)
(native char-downcase <char:char>)


;;;
;;;; 6.3.5 Strings
;;;


(native string? <object:bool>)
(native make-string <int^opt<char>:string>)
(native string <char*:string>)
(native string-length <string:int>)
(native string-ref <string^int:char>)
(native string-set! <string^int^char:void>)
(native string=? <string^string:bool>)
(native string-ci=? <string^string:bool>)
(native string<? <string^string:bool>)
(native string>? <string^string:bool>)
(native string<=? <string^string:bool>)
(native string>=? <string^string:bool>)
(native string-ci<? <string^string:bool>)
(native string-ci>? <string^string:bool>)
(native string-ci<=? <string^string:bool>)
(native string-ci>=? <string^string:bool>)
(native substring <string^int^int:string>)
(native string-append <string*:string>)
(native string->list <string:list>)
(native list->string <list:string>)
(native string-copy <string:string>)
(native string-fill! <string^char:void>)


;;;
;;;; 6.3.6 Vectors
;;;


(native vector? <object:bool>)
(native make-vector <int^opt<object>:vector>)
(native vector <object*:vector>)
(native vector-append <vector^vector:vector>)
(native vector-length <vector:int>)
(native vector-ref <vector^int:object>)
(native vector-set! <vector^int^object:void>)
(native vector->list <vector:list>)
(native list->vector <list:vector>)
(native vector-fill! <vector^object:void>)
(native subvector <vector^int^int:vector>)


;;;
;;;; 6.4 Control features
;;;


(native procedure? <object:bool>)
(native apply <procedure^object*:object>)
(native map <procedure^list*:list>)
(native for-each <procedure^list*:void>)
(native force <promise:object>)
(native call-with-current-continuation <procedure:object>)
(native call/cc <procedure:object>)
(native values <object*:object>)
(native call-with-values <procedure^procedure:object>)
(native dynamic-wind <procedure^procedure^procedure:object>)


;;;
;;;; 6.5 Eval
;;;


(native eval <object^object:object>)
(native scheme-replace-report-environment <object:object>)
(native null-environment <object:object>)
(native interaction-environment <:object>)


;;;
;;;; 6.6 Input and output
;;;


;;;
;;;; 6.6.1 Ports
;;;


(native call-with-input-file <string^procedure:object>)
(native call-with-output-file <string^procedure:object>)
(native input-port? <object:bool>)
(native output-port? <object:bool>)
(native current-input-port <:port>)
(native current-output-port <:port>)
(native with-input-from-file <string^procedure:object>)
(native with-output-to-file <string^procedure:object>)
(native open-input-file <string:port>)
(native open-output-file <string:port>)
(native close-input-port <port:void>)
(native close-output-port <port:void>)


;;;
;;;; 6.6.2 Input
;;;


(native read <opt<port>:object>)
(native read-char <opt<port>:object>)
(native peek-char <opt<port>:object>)
(native eof-object? <object:bool>)
(native char-ready? <opt<port>:bool>)


;;;
;;;; 6.6.3 Output
;;;


(native write <object^opt<port>:void>)
(native display <object^opt<port>:void>)
(native newline <opt<port>:void>)
(native write-char <char^opt<port>:void>)
(native load <string:void>)
(native transcript-on <string:void>)
(native transcript-off <:void>)


;;;
;;;; Syntactic Closure
;;;


(native make-syntactic-closure)
(native syntactic-closure?)
(native syntactic-closure-form)
(native unwrap-syntactic-closure)
(native strip-syntactic-closures)
(native strip-source-info)
(native identifier?)
(native identifier=?)
(native er-macro-transformer)
(native sc-macro-transformer)
(native rsc-macro-transformer)


;;;
;;;; Port
;;;


(native open-input-string)
(native open-output-string)
(native get-output-string)
(native call-with-input-string)
(native with-input-from-string)
(native call-with-output-string)
(native with-output-to-string)
(native read-line)


;;;
;;;; Table
;;;


(native table?)
(native make-table)
(native table-for-each)
(native table-ref)
(native table-set!)
(native table->list)
(native list->table)
(native jazz:iterate-table))

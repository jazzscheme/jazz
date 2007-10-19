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


(native eq? <any^any:bool>)
(native eqv? <any^any:bool>)
(native equal? <any^any:bool>)


;;;
;;;; 6.2 Numbers
;;;


;;;
;;;; 6.2.5 Numerical operations
;;;


(native number? <any:bool>)
(native complex? <any:bool>)
(native real? <any:bool>)
(native rational? <any:bool>)
(native integer? <any:bool>)
(native exact? <any:bool>)
(native inexact? <any:bool>)
(native = <:bool>)
(native < <:bool>)
(native > <:bool>)
(native <= <:bool>)
(native >= <:bool>)
(native zero? <any:bool>)
(native positive? <any:bool>)
(native negative? <any:bool>)
(native odd? <any:bool>)
(native even? <any:bool>)
(native max <:number>)
(native min <:number>)
(native + <:number>)
(native * <:number>)
(native - <:number>)
(native / <:number>)
(native abs <:number>)
(native quotient <:number>)
(native remainder <:number>)
(native modulo <:number>)
(native gcd <:number>)
(native lcm <:number>)
(native numerator <:number>)
(native denominator <:number>)
(native floor <:number>)
(native ceiling <:number>)
(native truncate <:number>)
(native round <:number>)
(native rationalize <:number>)
(native exp <:number>)
(native log <:number>)
(native sin <:number>)
(native cos <:number>)
(native tan <:number>)
(native asin <:number>)
(native acos <:number>)
(native atan <:number>)
(native sqrt <:number>)
(native expt <:number>)
(native make-rectangular <:number>)
(native make-polar <:number>)
(native real-part <:number>)
(native imag-part <:number>)
(native magnitude <:number>)
(native angle <:number>)
(native exact->inexact <:number>)
(native inexact->exact <:number>)


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


(native not <any:bool>)
(native boolean? <any:bool>)


;;;
;;;; 6.3.2 Pairs and lists
;;;


(native pair? <any:bool>)
(native cons <any^any:pair>)
(native car <pair:any>)
(native cdr <pair:any>)
(native set-car! <pair:void>)
(native set-cdr! <pair:void>)
(native caar <pair:any>)
(native cadr <pair:any>)
(native cdar <pair:any>)
(native cddr <pair:any>)
(native caaar <pair:any>)
(native caadr <pair:any>)
(native cadar <pair:any>)
(native caddr <pair:any>)
(native cdaar <pair:any>)
(native cdadr <pair:any>)
(native cddar <pair:any>)
(native cdddr <pair:any>)
(native caaaar <pair:any>)
(native caaadr <pair:any>)
(native caadar <pair:any>)
(native caaddr <pair:any>)
(native cadaar <pair:any>)
(native cadadr <pair:any>)
(native caddar <pair:any>)
(native cadddr <pair:any>)
(native cdaaar <pair:any>)
(native cdaadr <pair:any>)
(native cdadar <pair:any>)
(native cdaddr <pair:any>)
(native cddaar <pair:any>)
(native cddadr <pair:any>)
(native cdddar <pair:any>)
(native cddddr <pair:any>)
(native null? <any:bool>)
(native list? <any:bool>)
(native list <:list>)
(native length <list:int>)
(native append <:list>)
(native reverse <list:list>)
(native list-tail <list:list>)
(native list-ref <list:any>)
(native memq <list:list+>)
(native memv <list:list+>)
(native member <list:list+>)
(native assq <list:pair+>)
(native assv <list:pair+>)
(native assoc <list:pair+>)


;;;
;;;; 6.3.3 Symbols
;;;


(native symbol? <any:bool>)
(native symbol->string <symbol:string>)
(native string->symbol <string:symbol>)


;;;
;;;; 6.3.4 Characters
;;;


(native char? <any:bool>)
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
(native char->integer <char:int>)
(native integer->char <int:char>)
(native char-upcase <char:char>)
(native char-downcase <char:char>)


;;;
;;;; 6.3.5 Strings
;;;


(native string? <any:bool>)
(native make-string <:string>)
(native string <:string>)
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
(native string-append <:string>)
(native string->list <string:list>)
(native list->string <list:string>)
(native string-copy <string:string>)
(native string-fill! <string^char:void>)


;;;
;;;; 6.3.6 Vectors
;;;


(native vector? <any:bool>)
(native make-vector <:vector>)
(native vector <:vector>)
(native vector-length <vector:int>)
(native vector-ref <vector^int:any>)
(native vector-set! <vector^int^any:void>)
(native vector->list <vector:list>)
(native list->vector <list:vector>)
(native vector-fill! <vector^any:void>)


;;;
;;;; 6.4 Control features
;;;


(native procedure? <any:bool>)
(native apply <:any>)
(native map <:list>)
(native for-each <:void>)
(native force <:any>)
(native call-with-current-continuation <:any>)
(native call/cc <:any>)
(native values <:any>)
(native call-with-values <:any>)
(native dynamic-wind <:any>)


;;;
;;;; 6.5 Eval
;;;


(native eval <:any>)
(native scheme-replace-report-environment <:any>)
(native null-environment <:any>)
(native interaction-environment <:any>)


;;;
;;;; 6.6 Input and output
;;;


;;;
;;;; 6.6.1 Ports
;;;


(native call-with-input-file <:any>)
(native call-with-output-file <:any>)
(native input-port? <:bool>)
(native output-port? <:bool>)
(native current-input-port <:port>)
(native current-output-port <:port>)
(native with-input-from-file <:any>)
(native with-output-to-file <:any>)
(native open-input-file <:port>)
(native open-output-file <:port>)
(native close-input-port <:void>)
(native close-output-port <:void>)


;;;
;;;; 6.6.2 Input
;;;


(native read <:any>)
(native read-char <:any>)
(native peek-char <:any>)
(native eof-object? <:bool>)
(native char-ready? <:bool>)


;;;
;;;; 6.6.3 Output
;;;


(native write <:void>)
(native display <:void>)
(native newline <:void>)
(native write-char <:void>)
(native load <:void>)
(native transcript-on <:void>)
(native transcript-off <:void>))

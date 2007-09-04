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


(native eq?)
(native eqv?)
(native equal?)


;;;
;;;; 6.2 Numbers
;;;


;;;
;;;; 6.2.5 Numerical operations
;;;


(native number?)
(native complex?)
(native real?)
(native rational?)
(native integer?)
(native exact?)
(native inexact?)
(native =)
(native <)
(native >)
(native <=)
(native >=)
(native zero?)
(native positive?)
(native negative?)
(native odd?)
(native even?)
(native max)
(native min)
(native +)
(native *)
(native -)
(native /)
(native abs)
(native quotient)
(native remainder)
(native modulo)
(native gcd)
(native lcm)
(native numerator)
(native denominator)
(native floor)
(native ceiling)
(native truncate)
(native round)
(native rationalize)
(native exp)
(native log)
(native sin)
(native cos)
(native tan)
(native asin)
(native acos)
(native atan)
(native sqrt)
(native expt)
(native make-rectangular)
(native make-polar)
(native real-part)
(native imag-part)
(native magnitude)
(native angle)
(native exact->inexact)
(native inexact->exact)


;;;
;;;; 6.2.6 Numerical input and output
;;;


(native number->string)
(native string->number)


;;;
;;;; 6.3 Other data types
;;;


;;;
;;;; 6.3.1 Booleans
;;;


(native not)
(native boolean?)


;;;
;;;; 6.3.2 Pairs and lists
;;;


(native pair?)
(native cons)
(native car)
(native cdr)
(native set-car!)
(native set-cdr!)
(native caar)
(native cadr)
(native cdar)
(native cddr)
(native caaar)
(native caadr)
(native cadar)
(native caddr)
(native cdaar)
(native cdadr)
(native cddar)
(native cdddr)
(native caaaar)
(native caaadr)
(native caadar)
(native caaddr)
(native cadaar)
(native cadadr)
(native caddar)
(native cadddr)
(native cdaaar)
(native cdaadr)
(native cdadar)
(native cdaddr)
(native cddaar)
(native cddadr)
(native cdddar)
(native cddddr)
(native null?)
(native list?)
(native list)
(native length)
(native append)
(native reverse)
(native list-tail)
(native list-ref)
(native memq)
(native memv)
(native member)
(native assq)
(native assv)
(native assoc)


;;;
;;;; 6.3.3 Symbols
;;;


(native symbol?)
(native symbol->string)
(native string->symbol)


;;;
;;;; 6.3.4 Characters
;;;


(native char?)
(native char=?)
(native char<?)
(native char>?)
(native char<=?)
(native char>=?)
(native char-ci=?)
(native char-ci<?)
(native char-ci>?)
(native char-ci<=?)
(native char-ci>=?)
(native char-alphabetic?)
(native char-numeric?)
(native char-whitespace?)
(native char-upper-case?)
(native char-lower-case?)
(native char->integer)
(native integer->char)
(native char-upcase)
(native char-downcase)


;;;
;;;; 6.3.5 Strings
;;;


(native string?)
(native make-string)
(native string)
(native string-length)
(native string-ref)
(native string-set!)
(native string=?)
(native string-ci=?)
(native string<?)
(native string>?)
(native string<=?)
(native string>=?)
(native string-ci<?)
(native string-ci>?)
(native string-ci<=?)
(native string-ci>=?)
(native substring)
(native string-append)
(native string->list)
(native list->string)
(native string-copyy)
(native string-fill!)


;;;
;;;; 6.3.6 Vectors
;;;


(native vector?)
(native make-vector)
(native vector)
(native vector-length)
(native vector-ref)
(native vector-set!)
(native vector->list)
(native list->vector)
(native vector-fill!)


;;;
;;;; 6.4 Control features
;;;


(native procedure?)
(native apply)
(native map)
(native for-each)
(native force)
(native call-with-current-continuation)
(native call/cc)
(native values)
(native call-with-values)
(native dynamic-wind)


;;;
;;;; 6.5 Eval
;;;


(native eval)
(native scheme-replace-report-environment)
(native null-environment)
(native interaction-environment)


;;;
;;;; 6.6 Input and output
;;;


;;;
;;;; 6.6.1 Ports
;;;


(native call-with-input-file)
(native call-with-output-file)
(native input-port?)
(native output-port?)
(native current-input-port)
(native current-output-port)
(native with-input-from-file)
(native with-output-to-file)
(native open-input-file)
(native open-output-file)
(native close-input-port)
(native close-output-port)


;;;
;;;; 6.6.2 Input
;;;


(native read)
(native read-char)
(native peek-char)
(native eof-object?)
(native char-ready?)


;;;
;;;; 6.6.3 Output
;;;


(native write)
(native display)
(native newline)
(native write-char)
(native load)
(native transcript-on)
(native transcript-off)


;;;
;;;; 6.6.4 Instance
;;;

(native ##current-instance))

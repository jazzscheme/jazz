;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Development
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


(unit protected jazz.dialect.core.development


;;;
;;;; Global
;;;


(jazz:define-macro (jazz:define-global name)
  (let ((getter (%%string->symbol (%%string-append "get-" (%%symbol->string name))))
        (setter (%%string->symbol (%%string-append "set-" (%%symbol->string name))))
        (value (jazz:generate-symbol "value")))
    `(begin
       (define ,name
         #f)
       (define (,getter)
         ,name)
       (define (,setter ,value)
         (set! ,name ,value)))))


;;;
;;;; ? and %
;;;


(jazz:define-global ?)
(jazz:define-global %)


(jazz:define-global ?a)
(jazz:define-global ?b)
(jazz:define-global ?c)
(jazz:define-global ?d)
(jazz:define-global ?e)
(jazz:define-global ?f)
(jazz:define-global ?g)
(jazz:define-global ?h)
(jazz:define-global ?i)
(jazz:define-global ?j)
(jazz:define-global ?k)
(jazz:define-global ?l)
(jazz:define-global ?m)
(jazz:define-global ?n)
(jazz:define-global ?o)
(jazz:define-global ?p)
(jazz:define-global ?q)
(jazz:define-global ?r)
(jazz:define-global ?s)
(jazz:define-global ?t)
(jazz:define-global ?u)
(jazz:define-global ?v)
(jazz:define-global ?w)
(jazz:define-global ?x)
(jazz:define-global ?y)
(jazz:define-global ?z)


(jazz:define-global %a)
(jazz:define-global %b)
(jazz:define-global %c)
(jazz:define-global %d)
(jazz:define-global %e)
(jazz:define-global %f)
(jazz:define-global %g)
(jazz:define-global %h)
(jazz:define-global %i)
(jazz:define-global %j)
(jazz:define-global %k)
(jazz:define-global %l)
(jazz:define-global %m)
(jazz:define-global %n)
(jazz:define-global %o)
(jazz:define-global %p)
(jazz:define-global %q)
(jazz:define-global %r)
(jazz:define-global %s)
(jazz:define-global %t)
(jazz:define-global %u)
(jazz:define-global %v)
(jazz:define-global %w)
(jazz:define-global %x)
(jazz:define-global %y)
(jazz:define-global %z))

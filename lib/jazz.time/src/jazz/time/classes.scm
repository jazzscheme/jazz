;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Time Classes
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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


(unit protected jazz.time.classes


;;;
;;;; Time
;;;


(jazz:define-class time:Time-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-method (jazz:of-type? (time:Time-Class class) object)
  (time? object))


(jazz:define-method (jazz:emit-specifier (time:Time-Class class))
  'time)


(jazz:define-method (jazz:emit-test (time:Time-Class type) value source-declaration environment backend)
  `(time? ,value))


(jazz:define-method (jazz:write-object (time:Time-Class type) we obj)
  (jazz:print-value (jazz:find-dispatch type 'call-print) we obj))


(jazz:define-class time:Time jazz:Object (metaclass: time:Time-Class)
  ())


(%%table-set! jazz:primitive-types 'time time:Time)
(%%table-set! jazz:primitive-declarations time:Time 'Time)
(%%table-set! jazz:type-tests 'time? time:Time)
(jazz:register-structure-type time? time:Time)


;;;
;;;; Date
;;;


(jazz:define-class time:Date-Class jazz:Class (metaclass: jazz:Class)
  ())


(jazz:define-method (jazz:of-type? (time:Date-Class class) object)
  (date? object))


(jazz:define-method (jazz:emit-specifier (time:Date-Class class))
  'date)


(jazz:define-method (jazz:emit-test (time:Date-Class type) value source-declaration environment backend)
  `(date? ,value))


(jazz:define-method (jazz:write-object (time:Date-Class type) we obj)
  (jazz:print-value (jazz:find-dispatch type 'call-print) we obj))


(jazz:define-class time:Date jazz:Object (metaclass: time:Date-Class)
  ())


(%%table-set! jazz:primitive-types 'date time:Date)
(%%table-set! jazz:primitive-declarations time:Date 'Date)
(%%table-set! jazz:type-tests 'date? time:Date)
(jazz:register-structure-type date? time:Date))

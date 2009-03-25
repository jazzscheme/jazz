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


(module protected time.classes


;;;
;;;; Syntax
;;;


(jazz.define-class jazz.Time-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Time       jazz.Object () jazz.Time-Class () ())


(jazz.define-class jazz.Date-Class jazz.Class (name fields virtual-size ancestors descendants ascendant interfaces slots instance-size level dispatch-table core-method-alist core-virtual-alist core-virtual-names core-vtable class-table interface-table) jazz.Class () ())
(jazz.define-class jazz.Date       jazz.Object () jazz.Date-Class () ())


;;;
;;;; Time
;;;


(jazz.define-class-runtime jazz.Time-Class)


(jazz.define-method (jazz.of-type? (jazz.Time-Class class) object)
  (time? object))


(jazz.define-method (jazz.emit-specifier (jazz.Time-Class class))
  'time)


(jazz.define-method (jazz.emit-test (jazz.Time-Class type) value source-declaration environment)
  `(time? ,value))


(jazz.encapsulate-class jazz.Time-Class)


(jazz.define-class-runtime jazz.Time)


(jazz.encapsulate-class jazz.Time)


(%%table-set! jazz.primitive-types 'time jazz.Time)
(%%table-set! jazz.primitive-declarations jazz.Time 'Time)
(%%table-set! jazz.type-tests 'time? jazz.Time)
(jazz.register-usertype time? jazz.Time)


;;;
;;;; Date
;;;


(jazz.define-class-runtime jazz.Date-Class)


(jazz.define-method (jazz.of-type? (jazz.Date-Class class) object)
  (date? object))


(jazz.define-method (jazz.emit-specifier (jazz.Date-Class class))
  'date)


(jazz.define-method (jazz.emit-test (jazz.Date-Class type) value source-declaration environment)
  `(date? ,value))


(jazz.encapsulate-class jazz.Date-Class)


(jazz.define-class-runtime jazz.Date)


(jazz.encapsulate-class jazz.Date)


(%%table-set! jazz.primitive-types 'date jazz.Date)
(%%table-set! jazz.primitive-declarations jazz.Date 'Date)
(%%table-set! jazz.type-tests 'date? jazz.Date)
(jazz.register-usertype date? jazz.Date))

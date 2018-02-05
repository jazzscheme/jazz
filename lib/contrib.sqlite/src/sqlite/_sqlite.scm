;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; SQLite
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


(module sqlite scheme


(require (sqlite.implementation.resqlite3))


(native sqlite3-open)
(native sqlite3-close)
(native sqlite3-prepare)
(native sqlite3-step!)
(native sqlite3-reset!)
(native sqlite3-finalize!)

(native sqlite3-column-type)
(native sqlite3-column-name)
(native sqlite3-column-int)
(native sqlite3-column-int64)
(native sqlite3-column-double)
(native sqlite3-column-text)
(native sqlite3-column-u8vector)
(native sqlite3-column-s8vector)
(native sqlite3-column-u16vector)
(native sqlite3-column-s16vector)
(native sqlite3-column-u32vector)
(native sqlite3-column-s32vector)
(native sqlite3-column-u64vector)
(native sqlite3-column-s64vector)
(native sqlite3-column-f32vector)
(native sqlite3-column-f64vector)

(native sqlite3-bind-parameter-count)
(native sqlite3-bind-parameter-name)
(native sqlite3-bind-parameter-index)
(native sqlite3-bind-int!)
(native sqlite3-bind-int64!)
(native sqlite3-bind-double!)
(native sqlite3-bind-text!)
(native sqlite3-bind-blob!))

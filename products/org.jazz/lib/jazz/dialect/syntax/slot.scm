;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Slot Expansion
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2007
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


(in ?)


(class Slot-Expander extends Object uses (Interpreter)
  
  
  (definition meta unspecified
    (cons nil nil))
  
  
  @macro
  (slot s accessors generate)
  
  @expansion
  (declare s ()
    (%slot s)
    (attribute (:location (s 3))
      (method public (get-s)
        s))
    (attribute (:location (s 3))
      (method public (set-s __sym521)
        (set! s __sym521))))
  
  (method meta public (expand form)
    (bind-values (meta? modifiers rest) (parse-modifiers form)
      (let* ((name (car rest))
             (rest (cdr rest))
             (location (+ (length modifiers) 3))
             (typed? (and (pair? rest) (specifier? (car rest))))
             (type (if typed? (car rest) nil))
             (type-list (if type (list type) nil))
             (rest (if typed? (cdr rest) rest))
             (initialize (getf rest 'initialize :not-found unspecified))
             (default (getf rest 'default))
             (default-getter? (or (eq? default 'getter) (eq? default 'accessors)))
             (default-setter? (or (eq? default 'setter) (eq? default 'accessors)))
             (getter (if default-getter? (string->symbol (format "get-{a}" name)) nil))
             (setter (if default-setter? (string->symbol (format "set-{a}" name)) nil)))
        `(declare ,name ()
           (%slot ,@modifiers ,name ,@(if type (list type) nil)
                  ,@(if (neq? initialize unspecified) (list 'initialize initialize) nil))
           (attribute (:location (,name ,location))
             ,@(if default-getter? `((method public (,getter) ,name)) nil))
           (attribute (:location (,name ,location))
             ,@(if default-setter? (let ((symbol (generate-symbol))) `((method public (,setter ,symbol ,@type-list) (set! ,name ,symbol)))) nil)))))))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Enumeration Expansion
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


(class Enumeration-Expander extends Object uses (Interpreter)
  
  
  @macro
  (enumeration E
    (a 1)
    (b 2))
  
  @expansion
  (declare E spliced-enumeration-declaration
    (attribute (:location (E 2))
      (constant a 1))
    (attribute (:location (E 3))
      (constant b 2))
    (attribute (:location (E 1))
      (constant E
        (new Enumeration 'E
          (list
            (list 'a a)
            (list 'b b))))))
  
  @new
  (method meta public (expand form)
    (receive (meta? modifiers rest) (parse-modifiers form)
      (let ((base (+ (length modifiers) 1)))
        (bind (enumeration-name . items) rest
          (let ((declaration (new Enumeration-Declaration enumeration-name))
                (items (standardize-items items))
                (rank base))
            `(declare ,enumeration-name ,declaration
               ,@(map (function (item)
                        (increase! rank)
                        (bind (name value) item
                          `(attribute (:location (,enumeration-name ,rank))
                             (constant ,@(if meta? (list 'meta) #f) ,name ,value))))
                      items)
               (attribute (:location (,enumeration-name ,base))
                 (constant ,@modifiers ,enumeration-name
                   (new Enumeration ',enumeration-name
                     (list
                      ,@(map (function (item)
                               (bind (name) item
                                 `(list ',name ,name)))
                             items)))))))))))
  
  
  ;; old expand
  
  
  @macro
  (enumeration E
    (a 1)
    (b 2))
  
  @expansion
  (declare E ()
    (attribute (:location (E 1))
      (constant E <enumeration>))
    (attribute (:location (E 2))
      (constant a (set-enumeration! <enumeration> 'a 1)))
    (attribute (:location (E 2))
      (constant b (set-enumeration! <enumeration> 'b 2))))
  
  (method meta public (expand form)
    (receive (meta? modifiers rest) (parse-modifiers form)
      (let ((base (+ (length modifiers) 1)))
        (bind (enumeration-name . items) rest
          (let* ((items (standardize-items items))
                 (enumeration (new Enumeration enumeration-name (map (function (item) (list (car item) #f)) items)))
                 (rank base))
            `(declare ,enumeration-name ()
               (attribute (:location (,enumeration-name ,base))
                 (constant ,@modifiers ,enumeration-name ,enumeration))
               ,@(map (function (item)
                        (increase! rank)
                        (bind (name value) item
                          `(attribute (:location (,enumeration-name ,rank))
                             (constant ,@(if meta? (list 'meta) #f) ,name
                               (set-enumeration! ,enumeration ',name ,value)))))
                      items)))))))
  
  
  (method meta (standardize-items items)
    (let ((rank 0))
      (map (function (item)
             (prog1 (if (symbol? item)
                        (list item rank)
                      item)
               (increase! rank)))
           items))))

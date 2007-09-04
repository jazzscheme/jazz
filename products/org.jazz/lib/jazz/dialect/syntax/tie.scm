;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Tie Expansion
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


(class Tie-Expander extends Object


  (method meta public (expand objects)
    (typecase (car objects)
      ((String) (tie-string objects))
      @w
      ((List)   (tie-list   objects))
      (else   (error "Unable to tie: {t}" (car objects)))))
  
  
  ;;;
  ;;;; String
  ;;;
  
  
  (method meta (tie-string objects)
    (with-closed ((control (new String-Reader (apply append objects))))
      (let ((out (new String-Printer))
            (out-parameters (new List-Factory))
            (c))
        (while (neq? (set! c (read-char control)) #\eof)
          (case c
            ((#\~) (put (read-char control) out))
            ((#\{) (process-string control out out-parameters))
            (else (put c out))))
        (cons 'format (cons :string (cons (get-output~ out) (get-output~ out-parameters)))))))
  
  
  (method meta (process-string control out out-parameters)
    (bind (command . arguments) (read-delimited control "tie parameter" #\})
      (if (not (symbol? command))
          (error "Sorry, tie currently only accepts variables as parameters: {t}" command)
        (if (nil? arguments)
            (display "{a}" out)
          (format out "~{{l}}" arguments))
        (put~ out-parameters command))))
  
  
  (method meta (put c out)
    @w
    (when (memq? c '(#\~ #\{))
      (display "~" out))
    (format out "{c}" c))
  
  
  ;;;
  ;;;; List
  ;;;
  
  
  (method meta public (tie-list objects)
    (new Syntax-Expansion
      (car objects))))

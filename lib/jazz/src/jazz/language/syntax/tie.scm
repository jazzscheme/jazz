;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Tie
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


(module protected jazz.language.syntax.tie jazz.dialect


(import (jazz.language.runtime.kernel)
        (jazz.language.runtime.format)
        (jazz.language.runtime.functional))


(native private jazz:error)


(macro public (tie . objects)
  (define (process-char c out)
    (when (memq? c '(#\~ #\{))
      (display "~" out))
    (format out "{c}" c))
  
  (define (process-string control out out-parameters)
    (bind (command . arguments) (read-delimited control "tie parameter" #\})
      (if (not (symbol? command))
          (error "Tie currently only accepts variables as parameters: {t}" command)
        (if (null? arguments)
            (display "{a}" out)
          (format out "~{{l}}" arguments))
        (put out-parameters command))))
  
  (call-with-input-string (apply string-append objects)
    (lambda (control)
      (let ((out (open-output-string))
            (out-parameters (new List-Factory)))
        (let (iterate)
          (let ((c (read-char control)))
            (unless (eof-object? c)
              (case c
                ((#\~) (process-char (read-char control) out))
                ((#\{) (process-string control out out-parameters))
                (else (process-char c out)))
              (iterate))))
        (cons 'format (cons :string (cons (get-output-string out) (get-output out-parameters)))))))))

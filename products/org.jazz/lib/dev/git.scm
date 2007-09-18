;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Git
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


(library dev.git jazz


;;;
;;;; Patch
;;;


(define (read-patch file)
  (with-closed ((reader (new File-Reader :pathname file)))
    (define (skip-header)
      (let loop ()
        (bind-values (line proper?) (read-line reader)
          (if (starts-with? line "diff ")
              line
            (loop)))))
    
    (define (parse-diff diff)
      (let ((index (read-line reader))
            (a (read-line reader))
            (b (read-line reader))
            (range (read-line reader)))
        (bind-values (a-line a-range b-line b-range) (parse-range range)
          )))
    
    (define (parse-range range)
      (let* ((comma1 (find #\period range return: 'position)))
        ))
    
    (let ((line (skip-header)))
      (parse-diff line))))


;;;
;;;; Commands
;;;


(define (git arguments)
  (let ((port (open-process (list path: "git" arguments: arguments))))
    (pipe port (current-output-port))
    (close-port port)))


(define (git-add)
  (git (list "add")))


(define (git-move)
  #f)


(define (git-delete)
  #f)


(define (git-export)
  (git (list "format-patch")))


(define (git-import)
  (git (list "am")))


(define (git-branch)
  #f)


(define (git-checkout)
  #f)


(define (git-commit)
  #f)


(define (git-diff)
  #f)


(define (git-log)
  (git (list "log" "--decorate")))


(define (git-status)
  (git (list "status")))


(define (git-rollback)
  (git (list "reset" "--hard" "HEAD~1")))


(define (git-tag)
  (git (list "tag")))


(define (git-merge)
  #f)


(define (git-pull)
  #f)


(define (git-push)
  #f)


(define (pipe input output)
  (let loop ()
    (let ((c (read-char input)))
      (if (not (eof-object? c))
          (begin
            (write-char c output)
            (loop)))))))

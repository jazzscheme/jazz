;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Pathnames
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


(module core.jazz.runtime.system.pathname


;;;
;;;; Aliases
;;;


(define jazz.Aliases
  (%%new-hashtable ':eq?))


(define (jazz.get-aliases)
  jazz.Aliases)


(define (jazz.get-alias name)
  (or (%%hashtable-ref jazz.Aliases name #f)
      (jazz.error "Unknown alias: {s}" name)))


(define (jazz.add-alias name path)
  (%%hashtable-set! jazz.Aliases name path))


(jazz.add-alias 'Native '())


;;;
;;;; Pathname
;;;


(define (jazz.tokenise-filename filename)
  (%%cons 'Native (jazz.split-string filename #\/)))


(define (jazz.pathname-directory pathname)
  (jazz.butlast pathname))


(define (jazz.pathname-name pathname)
  (jazz.last pathname))


(define (jazz.pathname-base pathname)
  (jazz.filename-base (jazz.pathname-name pathname)))


(define (jazz.pathname-extension pathname)
  (jazz.filename-extension (jazz.pathname-name pathname)))


(define (jazz.filename-base filename)
  (let ((pos (jazz.find-char-reversed #\. filename)))
    (if pos
        (%%substring filename 0 pos)
      filename)))


(define (jazz.pathname-extend pathname name/base #!optional (extension #f))
  (let ((name (if extension (%%string-append name/base "." extension) name/base)))
    (%%append pathname (%%list name))))


(define (jazz.pathname-no-extension pathname)
  (jazz.pathname-extend (jazz.pathname-directory pathname) (jazz.pathname-base pathname)))


(define (jazz.pathname-brother pathname extension)
  (jazz.pathname-extend (jazz.pathname-directory pathname) (jazz.pathname-base pathname) extension))


(define (jazz.parse-pathname pathname)
  (let ((output (open-output-string))
        (first? #t))
    (letrec ((expand
              (lambda (object)
                (cond ((%%null? object)
                       )
                      ((%%pair? object)
                       (expand (%%car object))
                       (expand (%%cdr object)))
                      ((%%symbol? object)
                       (expand (jazz.get-alias object)))
                      ((%%string? object)
                       (if first?
                           (set! first? #f)
                         (display "/" output))
                       (display object output))))))
      (expand pathname)
      (get-output-string output)))))

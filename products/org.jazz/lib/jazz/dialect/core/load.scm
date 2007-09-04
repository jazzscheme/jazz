;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Runtime
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


(module core.jazz.runtime.runtime


;;;
;;;; Library
;;;

#;
(define (jazz.load-library module-name)
  (let ((module (jazz.get-environment-module module-name)))
    (cond ((%%not module)
           (let* ((pathname (jazz.need-manifest-source module-name))
                  ;; not needed atm
                  (core? #f #; (jazz.get-manifest-property module-name core?:))
                  (load-what (jazz.get-module-manifest-property (jazz.get-manifest-entry module-name) load: 'any)))
             (dynamic-wind
               (lambda ()
                 #f)
               (lambda ()
                 (let ((j (jazz.parse-pathname pathname))
                       (s (jazz.parse-pathname (jazz.pathname-brother pathname "jscm")))
                       (o (jazz.most-recent-object-file pathname)))
                   (let ((jtime (and (file-exists? j) (time->seconds (file-last-modification-time j))))
                         (stime (and (file-exists? s) (time->seconds (file-last-modification-time s))))
                         (otime (and (file-exists? o) (time->seconds (file-last-modification-time o)))))
                     (let ((kind (cond ((or (and otime (> otime jtime) (or (%%not stime) (> otime stime))))
                                        'compiled)
                                       ((or core? (and stime (> stime jtime)))
                                        'scheme)
                                       (else
                                        'interpreted))))
                       (if (and (%%not (%%eq? load-what 'any)) (%%not (%%eq? load-what kind)))
                           (jazz.error "File can only be loaded {s}: {s}" load-what j)
                         (case kind
                           ((compiled)
                            (jazz.load (jazz.parse-pathname (jazz.pathname-no-extension pathname))))
                           ((scheme)
                            (jazz.load s))
                           ((interpreted)
                            (cond (core?
                                   (jazz.compile-module module-name)
                                   (jazz.load s))
                                  (else
                                   (parameterize ((jazz.walk-context (jazz.new-walk-context 'debug module-name pathname)))
                                     (jazz.load j)))))))))))
               (lambda ()
                 (if (%%eq? (jazz.get-environment-module module-name) ':loading)
                     (jazz.set-environment-module module-name #f))))))
          ((%%eq? module ':loading)
           (jazz.error "Circular loading of {s}" module-name)))))


#;
(define (jazz.most-recent-object-file pathname)
  (let loop ((n 2)
             (last-path (jazz.parse-pathname (jazz.pathname-brother pathname "o1"))))
    (let ((new-path (jazz.parse-pathname (jazz.pathname-brother pathname (%%string-append "o" (number->string n))))))
      (if (file-exists? new-path)
          (loop (%%fixnum+ n 1) new-path)
        last-path))))


#;
(define (jazz.unload-module module-name)
  (%%hashtable-set! jazz.Catalog module-name #f)
  (jazz.set-environment-module module-name #f))


#;
(define (jazz.reload-module module-name)
  (jazz.unload-module module-name)
  (jazz.load-module module-name))


;;;
;;;; Unit
;;;


;; this has to be rethought
(define (jazz.load-unit unit-name)
  'todo))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Gambit Specific
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


(module jazz.dialect.core.gambit


;;;
;;;; Collector
;;;


(define jazz.gc ##gc)


;;;
;;;; Instance
;;;


(define jazz.current-instance ##current-instance)


;;;
;;;; Foreign
;;;


(define jazz.foreign? ##foreign?)
(define jazz.foreign-address foreign-address)
(define jazz.foreign-release! foreign-release!)
(define jazz.foreign-released? foreign-released?)
(define jazz.foreign-tag ##foreign-tag)
(define jazz.still-obj-refcount-dec! ##still-obj-refcount-dec!)
(define jazz.still-obj-refcount-inc! ##still-obj-refcount-inc!)
(define jazz.still-obj-refcount ##still-obj-refcount)


;;;
;;;; Port
;;;


(define jazz.close-port close-port)


;;;
;;;; Pathname
;;;


(define jazz.file-type file-type)
(define jazz._copy-file copy-file)
(define jazz.rename-file rename-file)
(define jazz.create-directory create-directory)
(define jazz.directory-files directory-files)


;;;
;;;; Thread
;;;


(define jazz.thread-sleep! thread-sleep!)


;;;
;;;; Statprof
;;;


(define jazz.statprof-loaded?
  #f)


(define profile-start! #f) (set! profile-start! #f)
(define profile-stop! #f) (set! profile-stop! #f)
(define write-profile-report #f) (set! write-profile-report #f)


(define (jazz.load-statprof)
  (if (not jazz.statprof-loaded?)
      (begin
        (load "../../contrib/statprof/statprof")
        (set! jazz.statprof-loaded? #t))))


(define (jazz.start-statprof)
  (jazz.load-statprof)
  (profile-start!))


(define (jazz.stop-statprof)
  (profile-stop!))


(define jazz.report-statprof
  (let ((n 0))
    (lambda ()
      (let ((port (open-output-string)))
        (display "STATPROF_REPORT_" port)
        (display n port)
        (set! n (+ n 1))
        (let ((name (get-output-string port)))
          (write-profile-report name)
          name)))))


;;;
;;;; System
;;;


(define system-exit exit))

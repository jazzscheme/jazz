;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Restricted
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


(block kernel.restricted


;;;
;;;; Authorized
;;;


(define jazz:filesystem-authorized-roots
  '())


(define (jazz:filesystem-authorize root)
  (if jazz:filesystem-restricted?
      (jazz:error "Restricted access to authorize")
    (if (%%not (jazz:filesystem-authorized? root))
        (set! jazz:filesystem-authorized-roots (append jazz:filesystem-authorized-roots (list root))))))


(define (jazz:filesystem-authorized? path)
  (jazz:some? (lambda (root)
                (jazz:string-starts-with? path root))
              jazz:filesystem-authorized-roots))


(define (jazz:filesystem-check path-or-settings)
  (let ((path (jazz:path-or-settings->path path-or-settings)))
    (if (not (jazz:filesystem-authorized? path))
        (jazz:error "Unauthorized access to path: {a}" path))))


(define (jazz:path-or-settings->path path-or-settings)
  (define (ill-formed)
    (jazz:error "Ill-formed path-or-settings: {s}" path-or-settings))
  
  (if (%%string? path-or-settings)
      path-or-settings
    (if (%%pair? path-or-settings)
        (if (and (%%eq? (%%car path-or-settings) 'path:)
                 (%%not (%%null? (%%cdr path-or-settings))))
            (%%cadr path-or-settings)
          (ill-formed))
      (ill-formed))))


(define (jazz:restrict-path-or-settings unrestricted)
  (lambda (path-or-settings . rest)
    (jazz:filesystem-check path-or-settings)
    (apply unrestricted path-or-settings rest)))


(define (jazz:restrict-transfer unrestricted)
  (lambda (from to . rest)
    (jazz:filesystem-check from)
    (jazz:filesystem-check to)
    (apply unrestricted from to rest)))


(define jazz:filesystem-unrestricted
  (%%make-table test: eq?))

(define jazz:filesystem-restricted
  (%%make-table test: eq?))


(define-macro (jazz:define-restricted name #!optional (restrict 'jazz:restrict-path-or-settings))
  `(begin
     (%%table-set! jazz:filesystem-unrestricted ',name ,name)
     (%%table-set! jazz:filesystem-restricted ',name (,restrict ,name))))


(define-macro (jazz:restrict name)
  `(set! ,name (%%table-ref jazz:filesystem-restricted ',name)))

(define-macro (jazz:unrestrict name)
  `(set! ,name (%%table-ref jazz:filesystem-unrestricted ',name)))


;;;
;;;; 13.2 Filesystem operations
;;;


(jazz:define-restricted create-directory)
(jazz:define-restricted create-fifo)
(jazz:define-restricted create-link)
(jazz:define-restricted create-symbolic-link)
(jazz:define-restricted rename-file)
(jazz:define-restricted copy-file jazz:restrict-transfer)
(jazz:define-restricted delete-file)
(jazz:define-restricted delete-directory)
(jazz:define-restricted directory-files)


;;;
;;;; 13.8 File information
;;;


(jazz:define-restricted file-exists?)
(jazz:define-restricted file-info)
(jazz:define-restricted file-type)
(jazz:define-restricted file-device)
(jazz:define-restricted file-inode)
(jazz:define-restricted file-mode)
(jazz:define-restricted file-number-of-links)
(jazz:define-restricted file-owner)
(jazz:define-restricted file-group)
(jazz:define-restricted file-size)
(jazz:define-restricted file-last-access-time)
(jazz:define-restricted file-last-modification-time)
(jazz:define-restricted file-last-change-time)
(jazz:define-restricted file-attributes)
(jazz:define-restricted file-creation-time)
(jazz:define-restricted file-last-access-and-modification-times-set!)


;;;
;;;; 14.7.1 Filesystem devices
;;;


(jazz:define-restricted open-file)
(jazz:define-restricted open-input-file)
(jazz:define-restricted open-output-file)
(jazz:define-restricted call-with-input-file)
(jazz:define-restricted call-with-output-file)
(jazz:define-restricted with-input-from-file)
(jazz:define-restricted with-output-to-file)


;;;
;;;; 14.8 Directory ports
;;;


(jazz:define-restricted open-directory)


;;;
;;;; Restricted
;;;


(define jazz:filesystem-restrictable?
  #f)

(define (jazz:filesystem-restrictable)
  (set! jazz:filesystem-restrictable? #t))


(define jazz:filesystem-restricted?
  #f)


(define (jazz:filesystem-allowed? path)
  (or (not jazz:filesystem-restricted?)
      (jazz:filesystem-authorized? path)))


(define (jazz:filesystem-restrict)
  (if (and jazz:filesystem-restrictable? (not jazz:filesystem-restricted?))
      (begin
        (jazz:restrict create-directory)
        (jazz:restrict create-fifo)
        (jazz:restrict create-link)
        (jazz:restrict create-symbolic-link)
        (jazz:restrict rename-file)
        (jazz:restrict copy-file)
        (jazz:restrict delete-file)
        (jazz:restrict delete-directory)
        (jazz:restrict directory-files)
        (jazz:restrict file-exists?)
        (jazz:restrict file-info)
        (jazz:restrict file-type)
        (jazz:restrict file-device)
        (jazz:restrict file-inode)
        (jazz:restrict file-mode)
        (jazz:restrict file-number-of-links)
        (jazz:restrict file-owner)
        (jazz:restrict file-group)
        (jazz:restrict file-size)
        (jazz:restrict file-last-access-time)
        (jazz:restrict file-last-modification-time)
        (jazz:restrict file-last-change-time)
        (jazz:restrict file-attributes)
        (jazz:restrict file-creation-time)
        (jazz:restrict file-last-access-and-modification-times-set!)
        (jazz:restrict open-file)
        (jazz:restrict open-input-file)
        (jazz:restrict open-output-file)
        (jazz:restrict call-with-input-file)
        (jazz:restrict call-with-output-file)
        (jazz:restrict with-input-from-file)
        (jazz:restrict with-output-to-file)
        (jazz:restrict open-directory)
        (set! jazz:filesystem-restricted? #t))))


(define (jazz:filesystem-unrestrict)
  (define (under-debugger?)
    (jazz:global-ref 'jazz.debuggee:controller-debugger))
  
  (if (and jazz:filesystem-restrictable? jazz:filesystem-restricted? (not (under-debugger?)))
      (begin
        (jazz:unrestrict create-directory)
        (jazz:unrestrict create-fifo)
        (jazz:unrestrict create-link)
        (jazz:unrestrict create-symbolic-link)
        (jazz:unrestrict rename-file)
        (jazz:unrestrict copy-file)
        (jazz:unrestrict delete-file)
        (jazz:unrestrict delete-directory)
        (jazz:unrestrict directory-files)
        (jazz:unrestrict file-exists?)
        (jazz:unrestrict file-info)
        (jazz:unrestrict file-type)
        (jazz:unrestrict file-device)
        (jazz:unrestrict file-inode)
        (jazz:unrestrict file-mode)
        (jazz:unrestrict file-number-of-links)
        (jazz:unrestrict file-owner)
        (jazz:unrestrict file-group)
        (jazz:unrestrict file-size)
        (jazz:unrestrict file-last-access-time)
        (jazz:unrestrict file-last-modification-time)
        (jazz:unrestrict file-last-change-time)
        (jazz:unrestrict file-attributes)
        (jazz:unrestrict file-creation-time)
        (jazz:unrestrict file-last-access-and-modification-times-set!)
        (jazz:unrestrict open-file)
        (jazz:unrestrict open-input-file)
        (jazz:unrestrict open-output-file)
        (jazz:unrestrict call-with-input-file)
        (jazz:unrestrict call-with-output-file)
        (jazz:unrestrict with-input-from-file)
        (jazz:unrestrict with-output-to-file)
        (jazz:unrestrict open-directory)
        (set! jazz:filesystem-restricted? #f)))))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Install
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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2008
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


(module jazz.install


;;;
;;;; Readtable
;;;


(define jazz.registered-jazz-literals
  '())

(define jazz.jazz-literals-installed?
  #f)


(define (jazz.register-jazz-literal name module constructor-name)
  (define (register)
    ((jazz.global-value 'jazz.register-literal-constructor)
     name
     (let ((constructor #f))
       (lambda rest
         (if (not constructor)
             (set! constructor (jazz.module-autoload module constructor-name)))
         (apply constructor rest)))))
  
  (if jazz.jazz-literals-installed?
      (register)
    (set! jazz.registered-jazz-literals (cons register jazz.registered-jazz-literals))))


(define jazz.install-jazz-literals
  (lambda ()
    (if jazz.jazz-literals-installed?
        (jazz.global-value 'jazz.jazz-readtable)
      (begin
        (jazz.load-module 'core.library)
        (jazz.load-module 'jazz.dialect)
        
        (for-each (lambda (literal)
                    (literal))
                  jazz.registered-jazz-literals)
        
        (set! jazz.registered-jazz-literals '())
        (set! jazz.jazz-literals-installed? #t)
      
        (jazz.global-value 'jazz.jazz-readtable)))))


;;;
;;;; Literals
;;;


(jazz.register-jazz-literal 'Point                     'jazz.literals 'construct-point)
(jazz.register-jazz-literal 'Dimension                 'jazz.literals 'construct-dimension)
(jazz.register-jazz-literal 'Cell                      'jazz.literals 'construct-cell)
(jazz.register-jazz-literal 'Rect                      'jazz.literals 'construct-rect)
(jazz.register-jazz-literal 'Range                     'jazz.literals 'construct-range)
(jazz.register-jazz-literal 'Exception-Detail          'jazz.literals 'construct-exception-detail)
(jazz.register-jazz-literal 'Walk-Location             'jazz.literals 'construct-walk-location)
(jazz.register-jazz-literal 'Box                       'jazz.literals 'construct-box)
(jazz.register-jazz-literal 'Action                    'jazz.literals 'construct-action)
(jazz.register-jazz-literal 'Shortcut                  'jazz.literals 'construct-shortcut)
(jazz.register-jazz-literal 'Locales                   'jazz.literals 'construct-locales)
(jazz.register-jazz-literal 'Color                     'jazz.literals 'construct-color)
(jazz.register-jazz-literal 'Font                      'jazz.literals 'construct-font)
(jazz.register-jazz-literal 'File                      'jazz.literals 'construct-file)
(jazz.register-jazz-literal 'Directory                 'jazz.literals 'construct-directory)
(jazz.register-jazz-literal 'Directory-Group           'jazz.literals 'construct-directory-group)
(jazz.register-jazz-literal 'Host                      'jazz.literals 'construct-host)
(jazz.register-jazz-literal 'IOR                       'jazz.literals 'construct-ior)
(jazz.register-jazz-literal 'Format                    'jazz.literals 'construct-format)
(jazz.register-jazz-literal 'Text-Style                'jazz.literals 'construct-text-style)
(jazz.register-jazz-literal 'Hyperlink-Style           'jazz.literals 'construct-hyperlink-style)
(jazz.register-jazz-literal 'Text                      'jazz.literals 'construct-text)
(jazz.register-jazz-literal 'Formatted-Text            'jazz.literals 'construct-formatted-text)
(jazz.register-jazz-literal 'Bitmap-Resource           'jazz.literals 'construct-bitmap-resource)
(jazz.register-jazz-literal 'Icon-Resource             'jazz.literals 'construct-icon-resource)
(jazz.register-jazz-literal 'Cursor-Resource           'jazz.literals 'construct-cursor-resource)
(jazz.register-jazz-literal 'Event                     'jazz.literals 'construct-event)
(jazz.register-jazz-literal 'Event-Handler             'jazz.literals 'construct-event-handler)
(jazz.register-jazz-literal 'Selection-Handler         'jazz.literals 'construct-selection-handler)
(jazz.register-jazz-literal 'Version                   'jazz.literals 'construct-version)
(jazz.register-jazz-literal 'C-File-Entry              'jazz.literals 'construct-c-file-entry)
(jazz.register-jazz-literal 'C-Category-Entry          'jazz.literals 'construct-c-category-entry)
(jazz.register-jazz-literal 'C-Define-Entry            'jazz.literals 'construct-c-define-entry)
(jazz.register-jazz-literal 'C-Include-Entry           'jazz.literals 'construct-c-include-entry)
(jazz.register-jazz-literal 'C-Export-Entry            'jazz.literals 'construct-c-export-entry)
;; wait (jazz.register-jazz-literal 'CSS-File-Entry            'jazz.literals 'construct-css-file-entry)
;; wait (jazz.register-jazz-literal 'CSS-Entry                 'jazz.literals 'construct-css-entry)
;; wait (jazz.register-jazz-literal 'Java-File-Entry           'jazz.literals 'construct-java-file-entry)
;; wait (jazz.register-jazz-literal 'JavaScript-File-Entry     'jazz.literals 'construct-javascript-file-entry)
;; wait (jazz.register-jazz-literal 'JavaScript-Variable-Entry 'jazz.literals 'construct-javascript-variable-entry)
;; wait (jazz.register-jazz-literal 'JavaScript-Function-Entry 'jazz.literals 'construct-javascript-function-entry)
;; wait (jazz.register-jazz-literal 'Lua-File-Entry            'jazz.literals 'construct-lua-file-entry)
;; wait (jazz.register-jazz-literal 'Lua-Function-Entry        'jazz.literals 'construct-lua-function-entry)
;; wait (jazz.register-jazz-literal 'Properties-File-Entry     'jazz.literals 'construct-properties-file-entry)
;; wait (jazz.register-jazz-literal 'Properties-Entry          'jazz.literals 'construct-properties-entry)
;; wait (jazz.register-jazz-literal 'Python-File-Entry         'jazz.literals 'construct-python-file-entry)
;; wait (jazz.register-jazz-literal 'Python-Class-Entry        'jazz.literals 'construct-python-class-entry)
;; wait (jazz.register-jazz-literal 'Python-Def-Entry          'jazz.literals 'construct-python-def-entry)
(jazz.register-jazz-literal 'Lisp-File-Entry           'jazz.literals 'construct-lisp-file-entry)
(jazz.register-jazz-literal 'Lisp-Entry                'jazz.literals 'construct-lisp-entry)


;;;
;;;; Extension
;;;


(jazz.register-reader-extension "jazz" jazz.install-jazz-literals))

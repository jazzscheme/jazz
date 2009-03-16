;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; All Product
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


(module jazz.all


;;;
;;;; Build
;;;


(define jazz.All-Modules
  '(jazz
    jazz.catalog
    jazz.console
    jazz.debuggee
    jazz.debugger
    jazz.debugger.debuggers.gambit
    jazz.debugger.debuggers.jazz
    jazz.designer
    jazz.doc
    jazz.dialect
    jazz.graphic
    jazz.graphic.font
    jazz.graphic.literals.colors
    jazz.graphic.literals.fonts
    jazz.groupware
    jazz.ide
    jazz.install
    jazz.io
    jazz.jml
    jazz.jrm
    jazz.language.c
    jazz.language.clike
    jazz.language.commonlisp
    jazz.language.csharp
    jazz.language.css
    jazz.language.diff
    jazz.language.html
    jazz.language.java
    jazz.language.javascript
    jazz.language.jazz
    jazz.language.jazz.debuggee
    jazz.language.jml
    jazz.language.lisp
    jazz.language.lua
    jazz.language.properties
    jazz.language.python
    jazz.language.scheme
    jazz.library
    jazz.library.component
    jazz.library.listener
    jazz.library.node
    jazz.literals
    jazz.media
    jazz.network
    jazz.platform
    jazz.platform.crash
    jazz.profile
    jazz.recorder
    jazz.runtime
    jazz.system
    jazz.system.application
    jazz.system.process
    jazz.ui
    jazz.ui.activity
    jazz.ui.clipboard
    jazz.ui.development
    jazz.ui.dialog
    jazz.ui.history
    jazz.ui.image
    jazz.ui.look
    jazz.ui.menu
    jazz.ui.offscreen
    jazz.ui.print
    jazz.ui.resizer
    jazz.ui.view
    jazz.ui.window
    jazz.ui.workspace
    jazz.utilities
    irregex.implementation.irregex
    statprof
    time
    time.implementation))


(define (jazz.build-all)
  (for-each jazz.build-module jazz.All-Modules))


;;;
;;;; Load
;;;


(define (jazz.load-all)
  (define (load module-name)
    (jazz.for-each-submodule module-name
      (lambda (module-name declaration phase)
        (jazz.load-module module-name))))
  
  (jazz.load-module 'core.library)
  (jazz.load-module 'core.module.builder)
  
  (for-each load jazz.All-Modules))


;;;
;;;; Register
;;;


(jazz.register-product 'all
  build: jazz.build-all))

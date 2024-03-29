;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Settings
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


(block kernel.settings


;;;
;;;; Jazz
;;;


(jazz:define-setting jazz:debug-expansion?
  #f)


;;;
;;;; Repositories
;;;


(jazz:define-setting jazz:build-repository
  #f)

(jazz:define-setting jazz:jazz-repository
  #f)

(jazz:define-setting jazz:repositories
  #f)

(jazz:define-setting jazz:dependencies
  #f)

(jazz:define-setting jazz:absolutize-sources?
  #f)


;;;
;;;; Verbose
;;;


(jazz:define-setting jazz:unit-verbose?
  #f)

(jazz:define-setting jazz:load-verbose?
  #f)

(jazz:define-setting jazz:outline-verbose?
  #f)

(jazz:define-setting jazz:shape-verbose?
  #f)

(jazz:define-setting jazz:done-verbose?
  #f)


;;;
;;;; Interpret
;;;


(jazz:define-setting jazz:warn-interpreted?
  #f)


;;;
;;;; Walker
;;;


;; code walker warnings
(jazz:define-setting jazz:warnings?
  #f)

;; set to #f to debug the walker itself
(jazz:define-setting jazz:delay-reporting?
  #t)


;;;
;;;; Print
;;;


;; low-level setting to disable calling print methods
(jazz:define-setting jazz:use-print?
  #t)


;;;
;;;; Build
;;;


(jazz:define-setting jazz:debug-build?
  #f)

(jazz:define-setting jazz:build-configure
  #f)

(jazz:define-setting jazz:build-setup
  #f)

(jazz:define-setting jazz:build-binary-repositories
  #f)

(jazz:define-setting jazz:build-source-access?
  #t)

(jazz:define-setting jazz:build-jazzini-access?
  #t)

(jazz:define-setting jazz:build-single-objects?
  #f)

(jazz:define-setting jazz:build-windows-homedir
  #f)

(jazz:define-setting jazz:build-link
  'objects)

(jazz:define-setting jazz:build-jobs
  #f)

(jazz:define-setting jazz:build-target
  #f)

(jazz:define-setting jazz:build-configuration
  #f)

(jazz:define-setting jazz:force-outlines?
  #f)

(jazz:define-setting jazz:dry-run?
  #f)

(jazz:define-setting jazz:save-emit?
  #f)


;;;
;;;; Debug
;;;


(jazz:define-setting jazz:debug-specializers
  '())


(jazz:define-setting jazz:force-interpreted?
  #f)


(jazz:define-setting jazz:use-debugger?
  #t)


(jazz:define-setting jazz:use-snapshot?
  #t)


;;;
;;;; Worker
;;;


(jazz:define-setting jazz:worker-repl?
  #f)


;;;
;;;; Profile
;;;


(jazz:define-setting jazz:profile-walker?
  #f)


;;;
;;;; Interface
;;;


(jazz:define-setting jazz:font-engine
  #f))

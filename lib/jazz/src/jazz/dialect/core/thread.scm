;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Threads
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


(module jazz.dialect.core.thread


;;;
;;;; Thread
;;;


(cond-expand
  (gambit
    (define jazz.current-thread current-thread)
    (define jazz.thread? thread?)
    (define jazz.make-thread make-thread)
    (define jazz.make-root-thread make-root-thread)
    (define jazz.thread-name thread-name)
    (define jazz.thread-specific thread-specific)
    (define jazz.thread-specific-set! thread-specific-set!)
    (define jazz.thread-base-priority thread-base-priority)
    (define jazz.thread-base-priority-set! thread-base-priority-set!)
    (define jazz.thread-priority-boost thread-priority-boost)
    (define jazz.thread-priority-boost-set! thread-priority-boost-set!)
    (define jazz.thread-start! thread-start!)
    (define jazz.thread-yield! thread-yield!)
    (define jazz.thread-sleep! thread-sleep!)
    (define jazz.thread-terminate! thread-terminate!)
    (define jazz.thread-join! thread-join!)
    (define jazz.thread-send thread-send)
    (define jazz.thread-receive thread-receive)
    (define jazz.thread-interrupt! thread-interrupt!)
    (define jazz.thread-thread-group thread-thread-group)
    (define jazz.thread-group->thread-group-list thread-group->thread-group-list)
    (define jazz.thread-group->thread-group-vector thread-group->thread-group-vector)
    (define jazz.thread-group->thread-list thread-group->thread-list)
    (define jazz.thread-group->thread-vector thread-group->thread-vector)
    (define jazz.thread-state thread-state)
    (define jazz.thread-state-abnormally-terminated-reason thread-state-abnormally-terminated-reason)
    (define jazz.thread-state-abnormally-terminated? thread-state-abnormally-terminated?)
    (define jazz.thread-state-active-timeout thread-state-active-timeout)
    (define jazz.thread-state-active-waiting-for thread-state-active-waiting-for)
    (define jazz.thread-state-active? thread-state-active?)
    (define jazz.thread-state-initialized? thread-state-initialized?)
    (define jazz.thread-state-normally-terminated-result thread-state-normally-terminated-result)
    (define jazz.thread-state-normally-terminated? thread-state-normally-terminated?)
    (define jazz.thread-state-uninitialized? thread-state-uninitialized?))
  
  (else))


;;;
;;;; Mutex
;;;


(cond-expand
  (gambit
    (define jazz.mutex? mutex?)
    (define jazz.make-mutex make-mutex)
    (define jazz.mutex-name mutex-name)
    (define jazz.mutex-specific mutex-specific)
    (define jazz.mutex-specific-set! mutex-specific-set!)
    (define jazz.mutex-state mutex-state)
    (define jazz.mutex-lock! mutex-lock!)
    (define jazz.mutex-unlock! mutex-unlock!)
    
    (define (jazz.mutex-wait mutex)
      (mutex-lock! mutex)
      (mutex-unlock! mutex)))
  
  (else))


;;;
;;;; Condition
;;;


(cond-expand
  (gambit
    (define jazz.condition? condition-variable?)
    (define jazz.make-condition make-condition-variable)
    (define jazz.condition-name condition-variable-name)
    (define jazz.condition-specific condition-variable-specific)
    (define jazz.condition-specific-set! condition-variable-specific-set!)
    (define jazz.condition-signal! condition-variable-signal!)
    (define jazz.condition-broadcast! condition-variable-broadcast!))
  
  (else)))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Kernel
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
;;;    Stephane Le Cornec
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


(library jazz.dialect.kernel scheme


;;;
;;;; Boolean
;;;


(native jazz.boolean)


;;;
;;;; Build
;;;


(native jazz.compile-file)
(native jazz.for-each-submodule)


;;;
;;;; Category
;;;


(native jazz.get-category-name)
(native jazz.get-class-ascendant)


;;;
;;;; Char
;;;


(native jazz.symbolic-char)
(native jazz.char-symbol)
(native jazz.whitespace?)


;;;
;;;; Collector
;;;


(native jazz.gc)


;;;
;;;; Continuation
;;;


(native jazz.continuation-capture)
(native jazz.continuation-graft)
(native jazz.continuation-return)


;;;
;;;; Debug
;;;


(native jazz.run-loop?)
(native jazz.debug)
(native jazz.error)
(native jazz.unimplemented)
(native jazz.dump-stack)
(native jazz.log-object)
(native jazz.log-string)
(native jazz.log-newline)
(native jazz.close-log)


;;;
;;;; Directory
;;;


(native jazz.get-current-directory)
(native jazz.set-current-directory)
(native jazz.with-current-directory)


;;;
;;;; Enumerator
;;;


(native jazz.enumerator?)


;;;
;;;; Exception
;;;

(native jazz.exception-reason)
(native jazz.current-exception-handler)
(native jazz.with-exception-handler)
(native jazz.with-exception-catcher)
(native jazz.with-default-exception-handler)
(native jazz.with-jazz-exception-handler)
(native jazz.dump-exception)
(native jazz.call-with-catch)
(native jazz.raise)


;;;
;;;; File
;;;


(native jazz.file-exists?)
(native jazz.file-delete)
(native jazz.file-last-modification-time)


;;;
;;;; Flixum
;;;


(native fixnum? <object:bool>)
(native flonum? <object:bool>)
(native jazz.fixnum->flonum <fx:fl>)
(native jazz.flonum->fixnum <fl:fx>)
(native fx+ <fx^fx:fx>)
(native fx- <fx^fx:fx>)
(native fx* <fx^fx:fx>)


;;;
;;;; Flonum
;;;


;; until proper call site casting of native calls

(native ##fl+ <fl^fl:fl>)
(native ##fl- <fl^fl:fl>)
(native ##fl* <fl^fl:fl>)
(native ##fl/ <fl^fl:fl>)


;;;
;;;; Format
;;;


(native jazz.lowlevel-format)


;;;
;;;; Identifier
;;;


(native jazz.composite-name?)
(native jazz.compose-name)
(native jazz.identifier-name)


;;;
;;;; Integer
;;;


(native bitwise-not <int:int>)
(native bitwise-and <int*:int>)
(native bitwise-ior <int*:int>)
(native bitwise-xor <int*:int>)
(native arithmetic-shift <int:int>)
(native bit-set? <int^int:bool>)


;;;
;;;; Key
;;;


(native jazz.apply-key)


;;;
;;;; Keyword
;;;


(native jazz.keyword?)
(native jazz.string->keyword)
(native jazz.keyword->string)


;;;
;;;; List
;;;


(native jazz.not-null?)
(native jazz.remove!)
(native jazz.remove-duplicates)
(native jazz.every?)
(native jazz.find-if)
(native jazz.collect-if)
(native jazz.collect-type)
(native jazz.partition)
(native jazz.reverse!)
(native jazz.last)
(native jazz.butlast)
(native jazz.listify)
(native jazz.list-copy)
(native jazz.last-pair)
(native jazz.getprop)
(native jazz.getf)
(native jazz.proper-list)


;;;
;;;; Number
;;;


(native jazz.naturals)


;;;
;;;; Object
;;;


(native jazz.new)
(native jazz.class-of)
(native jazz.object?)
(native jazz.category?)
(native jazz.class?)
(native jazz.interface?)
(native jazz.method?)
(native jazz.is?)
(native jazz.is-not?)
(native jazz.subtype?)
(native jazz.subcategory?)
(native jazz.subclass?)
(native jazz.load-module)
(native jazz.reload-module)


;;;
;;;; Field
;;;


(native jazz.field? <object:bool>)
(native jazz.field-name)
(native jazz.find-field)


;;;
;;;; Slot
;;;


(native jazz.slot? <object:bool>)
(native jazz.slot-value)
(native jazz.set-slot-value)


;;;
;;;; Property
;;;


(native jazz.property-getter)
(native jazz.property-setter)


;;;
;;;; Input
;;;


(native jazz.eof-object)
(native open-input-string)
(native call-with-input-string)


;;;
;;;; Output
;;;


(native jazz.print)
(native jazz.pretty-print)
(native jazz.format)
(native force-output)
(native jazz.->string)
(native open-output-string)
(native get-output-string)
(native call-with-output-string)


;;;
;;;; Pathname
;;;


(native jazz.file-type)
(native jazz._copy-file)
(native jazz.rename-file)
(native jazz.create-directory)
(native jazz.directory-files)


;;;
;;;; Port
;;;


(native port?)
(native jazz.close-port)


;;;
;;;; Process
;;;


(native open-process)
(native system-exit)


;;;
;;;; Queue
;;;


(native jazz.new-queue)
(native jazz.enqueue)
(native jazz.enqueue-list)
(native jazz.queue-list)
(native jazz.reset-queue)


;;;
;;;; Reader
;;;


(native skip-whites)
(native jazz.eof-object)
(native jazz.gambit-read-line)


;;;
;;;; Runtime
;;;


(native jazz.register-package)
(native jazz.get-environment)
(native jazz.get-catalog)
(native jazz.locate-library-declaration)
(native jazz.get-object-slot)
(native jazz.set-object-slot)
(native jazz.update-dispatch-table)
(native jazz.dispatch)
(native jazz.call-into-abstract)


;;;
;;;; Serial
;;;


(native jazz.object->serial-number)
(native jazz.serial-number->object)


;;;
;;;; Shell
;;;


(native shell-command)


;;;
;;;; Stack
;;;


(native jazz.continuation-capture)
(native jazz.get-continuation-stack)


;;;
;;;; Symbol
;;;


(native jazz.generate-symbol)
(native jazz.with-expression-value)


;;;
;;;; System
;;;


(native jazz.quit)


;;;
;;;; Table
;;;


(native table?)
(native make-table)
(native table-ref)
(native table-set!)
(native jazz.table-clear)
(native jazz.table-keys)
(native jazz.table-length)
(native jazz.iterate-table)
(native jazz.alist->table)
(native jazz.table-entries)
(native jazz.eq-hash)
(native jazz.eqv-hash)
(native jazz.equal-hash)


;;;
;;;; Thread
;;;


(native jazz.current-thread)
(native jazz.scheme-current-thread)
(native jazz.scheme-thread-start!)
(native jazz.make-scheme-thread)
(native jazz.thread.Thread.set-scheme-thread)
(native jazz.scheme.thread-specific-set!)
(native jazz.thread-start!)
(native jazz.thread-sleep!)


;;;
;;;; Time
;;;


(native jazz.running-time)
(native current-time)
(native time->seconds)
(native seconds->time)


;;;
;;;; Unspecified
;;;


(native jazz.unspecified)
(native jazz.unspecified?)
(native jazz.specified?)


;;;
;;;; Walker
;;;


(native jazz.new-walk-context)
(native jazz.register-literal-constructor)
(native jazz.specifier?)
(native jazz.parse-specifier)
(native jazz.requested-module-name)


;;;
;;;; Random
;;;


(native random-integer)
(native random-source-pseudo-randomize!)
(native default-random-source)


;;;
;;;; Parameters
;;;


(native make-parameter)
(native parameterize)


;;;
;;;; Autoload
;;;


(native jazz.autoload)
(native jazz.autoreload)


;;;
;;;; Foreign
;;;


(native jazz.foreign?)
(native jazz.foreign-address)
(native jazz.foreign-release!)
(native jazz.foreign-released?)
;;(native jazz.foreign-tag)
(native jazz.still-obj-refcount-dec!)
(native jazz.still-obj-refcount-inc!)
;;(native jazz.still-obj-refcount)


;;;
;;;; Instance
;;;


;;(native jazz.current-instance)


;;;
;;;; Statprof
;;;


(native jazz.start-statprof)
(native jazz.stop-statprof)
(native jazz.report-statprof))

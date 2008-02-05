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
;;;; App
;;;


(native jazz.app-username)


;;;
;;;; Autoload
;;;


(native jazz.autoload)
(native jazz.autoreload)


;;;
;;;; Boolean
;;;


(native jazz.boolean)


;;;
;;;; Build
;;;


(native jazz.compile-module)
(native jazz.compile-jazz-module)
(native jazz.for-each-submodule)


;;;
;;;; Category
;;;


(native jazz.get-category-name)
(native jazz.get-category-descendants)
(native jazz.get-class-ascendant)


;;;
;;;; Char
;;;


(native jazz.symbolic-char)
(native jazz.char-symbol)


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
;;;; Digest
;;;


(native open-digest)
(native close-digest)
(native digest-update-subu8vector)
(native digest-string)
(native digest-substring)
(native digest-u8vector)
(native digest-subu8vector)
(native digest-file)


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
(native jazz.raise)


;;;
;;;; Field
;;;


(native jazz.field? <object:bool>)
(native jazz.field-name)
(native jazz.find-field)


;;;
;;;; Fixnum
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
;;;; Foreign
;;;


(native jazz.foreign?)
(native jazz.foreign-address)
(native jazz.foreign-release!)
(native jazz.foreign-released?)
(native jazz.foreign-tags)
(native jazz.still-obj-refcount-dec!)
(native jazz.still-obj-refcount-inc!)
;;(native jazz.still-obj-refcount)


;;;
;;;; Identifier
;;;


(native jazz.composite-name?)
(native jazz.compose-name)
(native jazz.identifier-name)


;;;
;;;; Input
;;;


(native jazz.eof-object)
(native open-input-string)
(native call-with-input-string)
(native with-input-from-string)
(native jazz.read-line)
(native jazz.read-proper-line)
(native jazz.read-all)
(native jazz.with-extension-reader)


;;;
;;;; Instance
;;;


;;(native jazz.current-instance)


;;;
;;;; Integer
;;;


(native bitwise-not <int:int>)
(native bitwise-and <int*:int>)
(native bitwise-ior <int*:int>)
(native bitwise-xor <int*:int>)
(native arithmetic-shift <int:int>)
(native bit-set? <int^int:bool>)
(native extract-bit-field <int^int^int:int>)


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
(native jazz.listify)
(native jazz.list-copy)
(native jazz.last-pair)
(native jazz.proper-list)


;;;
;;;; Object
;;;


(native jazz.new)
(native jazz.class-of)
(native jazz.object?)
(native jazz.category?)
(native jazz.interface?)
(native jazz.method?)
(native jazz.is?)
(native jazz.subtype?)
(native jazz.subcategory?)
(native jazz.subclass?)
(native jazz.load-module)
(native jazz.reload-module)


;;;
;;;; Output
;;;


(native jazz.print)
(native jazz.pretty-print)
(native force-output)
(native open-output-string)
(native get-output-string)
(native call-with-output-string)
(native current-error-port)
(native with-output-to-port)
(native write-u8)


;;;
;;;; Parameters
;;;


(native make-parameter)
(native parameterize)


;;;
;;;; Pathname
;;;


(native jazz.pathname-type)
(native jazz.pathname-expand)
(native jazz.pathname-normalize)
(native jazz.file-exists?)
(native jazz.file-delete)
(native jazz.file-copy)
(native jazz.file-modification-time)
(native jazz.file-rename)
(native jazz.directory-create)
(native jazz.directory-content)
(native jazz.directory-delete)
(native jazz.get-current-directory)
(native jazz.set-current-directory)
(native jazz.with-current-directory)


;;;
;;;; Port
;;;


(native port?)
(native jazz.close-port)


;;;
;;;; Property
;;;


(native jazz.property-getter)
(native jazz.property-setter)


;;;
;;;; Queue
;;;


(native jazz.new-queue)
(native jazz.enqueue)
(native jazz.enqueue-list)
(native jazz.queue-list)
(native jazz.reset-queue)


;;;
;;;; Random
;;;


(native jazz.random-integer)
(native jazz.random-source-pseudo-randomize!)
(native jazz.default-random-source)


;;;
;;;; Resource
;;;


(native jazz.resource-pathname)


;;;
;;;; Runtime
;;;


(native jazz.jazz-directory)
(native jazz.reset-packages)
(native jazz.find-module-src)
(native jazz.get-environment)
(native jazz.get-catalog)
(native jazz.locate-library-declaration)
(native jazz.get-object-slot)
(native jazz.set-object-slot)
(native jazz.dispatch)
(native jazz.find-dispatch)
(native jazz.call-into-abstract)
(native jazz.debug?)


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
;;;; Slot
;;;


(native jazz.slot? <object:bool>)
(native jazz.slot-value)
(native jazz.set-slot-value)


;;;
;;;; Socket
;;;


(native jazz.open-tcp-client)
(native jazz.open-tcp-server)


;;;
;;;; Stack
;;;


(native jazz.continuation-capture)
(native jazz.get-continuation-stack)


;;;
;;;; Statprof
;;;


(native jazz.start-statprof)
(native jazz.stop-statprof)
(native jazz.reset-statprof)
(native jazz.report-statprof)


;;;
;;;; String
;;;


(native jazz.join-strings)


;;;
;;;; Symbol
;;;


(native jazz.generate-symbol)
(native jazz.with-expression-value)


;;;
;;;; System
;;;


(native open-process)
(native jazz.exit)


;;;
;;;; Table
;;;


(native table?)
(native make-table)
(native table-ref)
(native table-set!)
(native table->list)
(native list->table)
(native jazz.table-clear)
(native jazz.table-length)
(native jazz.iterate-table)
(native jazz.table-entries)
(native jazz.eq-hash)
(native jazz.eqv-hash)
(native jazz.equal-hash)


;;;
;;;; Thread
;;;


(native jazz.current-thread)
(native jazz.thread?)
(native jazz.make-thread)
(native jazz.thread-name)
(native jazz.thread-specific)
(native jazz.thread-specific-set!)
(native jazz.thread-start!)
(native jazz.thread-yield!)
(native jazz.thread-sleep!)
(native jazz.thread-terminate!)
(native jazz.thread-join!)
(native jazz.thread-send)
(native jazz.thread-receive)
(native jazz.mutex?)
(native jazz.make-thread)
(native jazz.mutex-name)
(native jazz.mutex-specific)
(native jazz.mutex-specific-set!)
(native jazz.mutex-state)
(native jazz.mutex-lock!)
(native jazz.mutex-unlock!)


;;;
;;;; Time
;;;


(native jazz.process-times)
(native jazz.cpu-time)
(native jazz.real-time)


;;;
;;;; Unspecified
;;;


(native jazz.unspecified)
(native jazz.unspecified?)
(native jazz.specified?)


;;;
;;;; Vector
;;;


(native jazz.vector-copy)


;;;
;;;; Walker
;;;


(native jazz.new-walk-context)
(native jazz.register-literal-constructor)
(native jazz.specifier?)
(native jazz.parse-specifier)
(native jazz.requested-module-name))

;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel 0
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


(library jazz.dialect.kernel.kernel0 scheme


;;;
;;;; Boolean
;;;


(native jazz.boolean)


;;;
;;;; Build
;;;


(native jazz.compile-file)


;;;
;;;; Char
;;;


(native jazz.symbolic-char)
(native jazz.char-symbol)
(native jazz.whitespace?)


;;;
;;;; Debug
;;;


(native jazz.debug)
(native jazz.error)
(native jazz.unimplemented)
(native jazz.dump-stack)


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
(native jazz.string->enumerator)
(native jazz.enumerator->string)


;;;
;;;; Exception
;;;

(native jazz.exception-reason)
(native jazz.with-exception-handler)
(native jazz.with-default-exception-handler)
(native jazz.with-jazz-exception-handler)
(native jazz.dump-exception)
(native jazz.call-with-catch)


(native with-exception-catcher)
(native raise)


;;;
;;;; File
;;;


(native jazz.file-exists?)
(native jazz.file-delete)
(native jazz.file-last-modification-time)


;;;
;;;; Format
;;;


(native jazz.lowlevel-format)


;;;
;;;; Hashtable
;;;


(native jazz.make-hashtable)
(native jazz.hashtable-ref)
(native jazz.hashtable-set!)
(native jazz.hashtable-clear)
(native jazz.hashtable-keys)
(native jazz.iterate-hashtable)
(native jazz.alist->hashtable)
(native jazz.hashtable-entries)
(native jazz.eq-hash)
(native jazz.eqv-hash)
(native jazz.equal-hash)


;;;
;;;; Identifier
;;;


(native jazz.composite-name?)
(native jazz.identifier-name)


;;;
;;;; Integer
;;;


(native jazz.bit-or)
(native bitwise-not)
(native bitwise-and)
(native bitwise-ior)
(native bitwise-xor)
(native arithmetic-shift)
(native bit-set?)


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
(native jazz.find-keyword)


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
;;;; Nil
;;;


(native jazz.NilConstant)


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
(native jazz.unit?)
(native jazz.class?)
(native jazz.interface?)
(native jazz.method?)
(native jazz.is?)
(native jazz.is-not?)
(native jazz.subtype?)
(native jazz.load-module)
(native jazz.load-metaclass)
(native jazz.reload-module)


;;;
;;;; Field
;;;


(native jazz.field?)
(native jazz.field-name)
(native jazz.find-field)


;;;
;;;; Slot
;;;


(native jazz.slot?)
(native jazz.slot-value)
(native jazz.set-slot-value)


;;;
;;;; Property
;;;


(native jazz.property-getter)
(native jazz.property-setter)


;;;
;;;; Units
;;;


(native jazz.get-unit-name)
(native jazz.get-class-ascendant)


;;;
;;;; Input
;;;


(native open-input-string)


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


;;;
;;;; Pathname
;;;


(native jazz.tokenise-filename)
(native jazz.pathname-directory)
(native jazz.pathname-name)
(native jazz.pathname-base)
(native jazz.pathname-extension)
(native jazz.filename-extension)
(native jazz.filename-base)
(native jazz.pathname-extend)
(native jazz.pathname-brother)
(native jazz.parse-pathname)


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


;;;
;;;; Reader
;;;


(native skip-whites)



;;;
;;;; Runtime
;;;


(native jazz.get-environment)
(native jazz.get-catalog)
(native jazz.locate-library-declaration)
(native jazz.load-module-declaration)
(native jazz.read-module-declaration)
(native jazz.expand-toplevel-form)
(native jazz.get-object-slot)
(native jazz.update-dispatch-table)
(native jazz.need-dispatch-implementation-by-name)


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
;;;; String
;;;


(native jazz.split-string)
(native jazz.join-strings)


;;;
;;;; Symbol
;;;


(native jazz.generate-symbol)
(native symbol->keyword)
(native keyword->symbol)


;;;
;;;; System
;;;


(native jazz.quit)


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


;;;
;;;; Time
;;;


(native jazz.running-time)
(native current-time)
(native time->seconds)


;;;
;;;; Void
;;;


(native jazz.void)
(native jazz.void?)
(native jazz.not-void?)


;;;
;;;; Walk
;;;


(native jazz.new-walk-context)
(native jazz.register-literal-constructor)


;;;
;;;; Random
;;;


(native random-integer)
(native random-source-pseudo-randomize!)
(native default-random-source)


;;;
;;;; Number
;;;


(native min)
(native max)


;;;
;;;; Parameters
;;;


(native make-parameter)
(native parameterize)


;;;
;;;; Table
;;;


(native for-each-table)


;;;
;;;; EOF
;;;


(native jazz.eof-object)


;;;
;;;; autoload
;;;


(native jazz.autoload)
(native jazz.autoreload)
(native jazz.foreign-address)
(native jazz.foreign?)
(native current-instance))

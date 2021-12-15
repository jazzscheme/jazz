;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Gambit Language Runtime
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


(module protected gambit.language.runtime scheme


;;;
;;;; Box
;;;


(native box)
(native box?)
(native set-box!)
(native unbox)


;;;
;;;; Command
;;;


(native command-line)


;;;
;;;; Compile
;;;


(native compile-file)
(native compile-file-to-target)
(native link-flat)
(native link-incremental)


;;;
;;;; Condition
;;;


(native condition-variable-broadcast!)
(native condition-variable-name)
(native condition-variable-signal!)
(native condition-variable-specific)
(native condition-variable-specific-set!)
(native condition-variable?)
(native make-condition-variable)


;;;
;;;; Continuation
;;;


(native continuation-capture)
(native continuation-graft)
(native continuation-return)
(native continuation?)


;;;
;;;; Debugging
;;;


(native break)
(native display-continuation-backtrace)
(native display-continuation-dynamic-environment)
(native display-continuation-environment)
(native display-dynamic-environment?)
(native display-environment-set!)
(native display-exception)
(native display-exception-in-context)
(native display-procedure-environment)
(native generate-proper-tail-calls)
(native step)
(native step-level-set!)
(native trace)
(native unbreak)
(native untrace)


;;;
;;;; Environment
;;;


(native getenv)
(native setenv)


;;;
;;;; Evaluation
;;;


(native expression-parsing-exception-kind)
(native expression-parsing-exception-parameters)
(native expression-parsing-exception-source)
(native expression-parsing-exception?)
(native unbound-global-exception-code)
(native unbound-global-exception-rte)
(native unbound-global-exception-variable)
(native unbound-global-exception?)
(native uncaught-exception-arguments)
(native uncaught-exception-procedure)
(native uncaught-exception-reason)
(native uncaught-exception?)


;;;
;;;; Exception
;;;


(native abort)
(native current-exception-handler)
(native error)
(native raise)
(native with-exception-catcher)
(native with-exception-handler)
(native error-exception-message)
(native error-exception-parameters)
(native error-exception?)
(native noncontinuable-exception-reason)
(native noncontinuable-exception?)
(native primordial-exception-handler)


;;;
;;;; File
;;;


(native create-directory)
(native create-fifo)
(native create-link)
(native create-symbolic-link)
(native current-directory)
(native delete-directory)
(native delete-file)
(native directory-files)
(native file-attributes)
(native file-creation-time)
(native file-device)
(native file-exists?)
(native file-group)
(native file-info)
(native file-info-attributes)
(native file-info-creation-time)
(native file-info-device)
(native file-info-group)
(native file-info-inode)
(native file-info-last-access-time)
(native file-info-last-change-time)
(native file-info-last-modification-time)
(native file-info-mode)
(native file-info-number-of-links)
(native file-info-owner)
(native file-info-size)
(native file-info-type)
(native file-info?)
(native file-inode)
(native file-last-access-time)
(native file-last-change-time)
(native file-last-modification-time)
(native file-last-access-and-modification-times-set!)
(native file-mode)
(native file-number-of-links)
(native file-owner)
(native file-size)
(native file-type)
(native rename-file)


;;;
;;;; Fixnum
;;;


(native fx+ <fx^fx:fx>)
(native fx- <fx^fx:fx>)
(native fx* <fx^fx:fx>)
(native fx<)
(native fx<=)
(native fx=)
(native fx>)
(native fx>=)
(native fxabs)
(native fxand)
(native fxarithmetic-shift)
(native fxarithmetic-shift-left)
(native fxarithmetic-shift-right)
(native fxbit-count)
(native fxbit-set?)
(native fxeven?)
(native fxfirst-bit-set)
(native fxif)
(native fxior)
(native fxlength)
(native fxmax)
(native fxmin)
(native fxmodulo)
(native fxnegative?)
(native fxnot)
(native fxodd?)
(native fxpositive?)
(native fxquotient)
(native fxremainder)
(native fxwrap*)
(native fxwrap+)
(native fxwrap-)
(native fxwrapabs)
(native fxwraparithmetic-shift)
(native fxwraparithmetic-shift-left)
(native fxwraplogical-shift-right)
(native fxwrapquotient)
(native fxxor)
(native fxzero?)
(native fixnum-overflow-exception-arguments)
(native fixnum-overflow-exception-procedure)
(native fixnum-overflow-exception?)
(native fixnum? <object:bool>)


;;;
;;;; Flonum
;;;


(native fl*)
(native fl+)
(native fl-)
(native fl/)
(native fl<)
(native fl<=)
(native fl=)
(native fl>)
(native fl>=)
(native flabs)
(native flacos)
(native flasin)
(native flatan)
(native flceiling)
(native flcos)
(native fldenominator)
(native fleven?)
(native flexp)
(native flexpt)
(native flfinite?)
(native flfloor)
(native flinfinite?)
(native flinteger?)
(native fllog)
(native flmax)
(native flmin)
(native flnan?)
(native flnegative?)
(native flnumerator)
(native flodd?)
(native flonum? <object:bool>)
(native flpositive?)
(native flround)
(native flsin)
(native flsqrt)
(native fltan)
(native fltruncate)
(native flzero?)


;;;
;;;; Foreign
;;;


(native foreign-address)
(native foreign-release!)
(native foreign-released?)
(native foreign-tags)
(native foreign?)
(native cfun-conversion-exception-arguments)
(native cfun-conversion-exception-code)
(native cfun-conversion-exception-message)
(native cfun-conversion-exception-procedure)
(native cfun-conversion-exception?)
(native multiple-c-return-exception?)
(native sfun-conversion-exception-arguments)
(native sfun-conversion-exception-code)
(native sfun-conversion-exception-message)
(native sfun-conversion-exception-procedure)
(native sfun-conversion-exception?)


;;;
;;;; Group
;;;


(native group-info)
(native group-info-gid)
(native group-info-members)
(native group-info-name)
(native group-info?)


;;;
;;;; Hash
;;;


(native eq?-hash)
(native equal?-hash)
(native eqv?-hash)
(native keyword-hash)
(native symbol-hash)
(native string=?-hash)
(native string-ci=?-hash)
(native object->serial-number)
(native serial-number->object)
(native invalid-hash-number-exception-arguments)
(native invalid-hash-number-exception-procedure)
(native invalid-hash-number-exception?)
(native unbound-serial-number-exception-arguments)
(native unbound-serial-number-exception-procedure)
(native unbound-serial-number-exception?)


;;;
;;;; Homogeneous
;;;


;; s8
(native s8vector <:s8vector>)
(native make-s8vector <fx^opt<int>:s8vector>)
(native s8vector-length)
(native s8vector-ref <s8vector^fx:fx>)
(native s8vector-set!)
(native s8vector?)
(native s8vector->list)
(native list->s8vector)
(native s8vector-append)
(native s8vector-copy)
(native s8vector-fill!)
(native subs8vector)
(native subs8vector-fill!)
(native subs8vector-move!)

;; u8
(native u8vector <:u8vector>)
(native make-u8vector <fx^opt<int>:u8vector>)
(native u8vector-length)
(native u8vector-ref <u8vector^fx:fx>)
(native u8vector-set!)
(native u8vector?)
(native u8vector->list)
(native list->u8vector)
(native u8vector->object)
(native object->u8vector)
(native object->u8vector)
(native u8vector-shrink!)
(native u8vector-append)
(native u8vector-copy)
(native u8vector-fill!)
(native subu8vector)
(native subu8vector-fill!)
(native subu8vector-move!)

;; s16
(native s16vector <:s16vector>)
(native make-s16vector <fx^opt<int>:s16vector>)
(native s16vector-length)
(native s16vector-ref <s16vector^fx:fx>)
(native s16vector-set!)
(native s16vector?)
(native s16vector->list)
(native list->s16vector)
(native s16vector-append)
(native s16vector-copy)
(native s16vector-fill!)
(native subs16vector)
(native subs16vector-fill!)
(native subs16vector-move!)

;; u16
(native u16vector <:u16vector>)
(native make-u16vector <fx^opt<int>:u16vector>)
(native u16vector-length)
(native u16vector-ref <u16vector^fx:fx>)
(native u16vector-set!)
(native u16vector?)
(native u16vector->list)
(native u16vector-fill!)
(native list->u16vector)
(native u16vector-append)
(native u16vector-copy)
(native u16vector-fill!)
(native subu16vector)
(native subu16vector-fill!)
(native subu16vector-move!)

;; s32
(native s32vector <:s32vector>)
(native make-s32vector <fx^opt<int>:s32vector>)
(native s32vector-length)
(native s32vector-ref <s32vector^fx:fx>)
(native s32vector-set!)
(native s32vector?)
(native s32vector->list)
(native list->s32vector)
(native s32vector-append)
(native s32vector-copy)
(native s32vector-fill!)
(native subs32vector)
(native subs32vector-fill!)
(native subs32vector-move!)

;; u32
(native u32vector <:u32vector>)
(native make-u32vector <fx^opt<int>:u32vector>)
(native u32vector-length)
(native u32vector-ref <u32vector^fx:fx>)
(native u32vector-set!)
(native u32vector?)
(native u32vector->list)
(native list->u32vector)
(native u32vector-append)
(native u32vector-copy)
(native u32vector-fill!)
(native subu32vector)
(native subu32vector-fill!)
(native subu32vector-move!)

;; s64
(native s64vector <:s64vector>)
(native make-s64vector <fx^opt<int>:s64vector>)
(native s64vector-length)
(native s64vector-ref)
(native s64vector-set!)
(native s64vector?)
(native s64vector->list)
(native list->s64vector)
(native s64vector-append)
(native s64vector-copy)
(native s64vector-fill!)
(native subs64vector)
(native subs64vector-fill!)
(native subs64vector-move!)

;; u64
(native u64vector <:u64vector>)
(native make-u64vector <fx^opt<int>:u64vector>)
(native u64vector-length)
(native u64vector-ref)
(native u64vector-set!)
(native u64vector?)
(native u64vector->list)
(native list->u64vector)
(native u64vector-append)
(native u64vector-copy)
(native u64vector-fill!)
(native subu64vector)
(native subu64vector-fill!)
(native subu64vector-move!)

;; f32
(native f32vector <:f32vector>)
(native make-f32vector <fx^opt<fl>:f32vector>)
(native f32vector-length)
(native f32vector-ref <f32vector^fx:fl>)
(native f32vector-set!)
(native f32vector?)
(native f32vector->list)
(native list->f32vector)
(native f32vector-append)
(native f32vector-copy)
(native f32vector-fill!)
(native subf32vector)
(native subf32vector-fill!)
(native subf32vector-move!)

;; f64
(native f64vector <:f64vector>)
(native make-f64vector <fx^opt<fl>:f64vector>)
(native f64vector-length)
(native f64vector-ref <f64vector^fx:fl>)
(native f64vector-set!)
(native f64vector?)
(native f64vector->list)
(native list->f64vector)
(native f64vector-append)
(native f64vector-copy)
(native f64vector-fill!)
(native subf64vector)
(native subf64vector-fill!)
(native subf64vector-move!)


;;;
;;;; Host
;;;


(native address-infos)
(native address-info-family)
(native address-info-protocol)
(native address-info-socket-info)
(native address-info-socket-type)
(native address-info?)
(native err-code->string)
(native host-info)
(native host-info-addresses)
(native host-info-aliases)
(native host-info-name)
(native host-info?)
(native host-name)
(native no-such-file-or-directory-exception-arguments)
(native no-such-file-or-directory-exception-procedure)
(native no-such-file-or-directory-exception?)
(native os-exception-arguments)
(native os-exception-code)
(native os-exception-message)
(native os-exception-procedure)
(native os-exception?)
(native unbound-os-environment-variable-exception-arguments)
(native unbound-os-environment-variable-exception-procedure)
(native unbound-os-environment-variable-exception?)


;;;
;;;; Integer
;;;


(native integer-nth-root)
(native integer-sqrt)


;;;
;;;; Interrupt
;;;


(native current-user-interrupt-handler)
(native defer-user-interrupts)


;;;
;;;; Keyword
;;;


(native make-uninterned-keyword)
(native keyword->string)
(native keyword-expected-exception-arguments)
(native keyword-expected-exception-procedure)
(native keyword-expected-exception?)
(native keyword?)
(native string->keyword)
(native uninterned-keyword?)


;;;
;;;; Memory
;;;


(native gc-report-set!)
(native heap-overflow-exception?)
(native stack-overflow-exception?)


;;;
;;;; Mutex
;;;


(native make-mutex)
(native mutex-lock!)
(native mutex-name)
(native mutex-specific)
(native mutex-specific-set!)
(native mutex-state)
(native mutex-unlock!)
(native mutex?)
(native abandoned-mutex-exception?)


;;;
;;;; Network
;;;


(native network-info)
(native network-info-aliases)
(native network-info-name)
(native network-info-number)
(native network-info?)


;;;
;;;; Bitwise
;;;


(native all-bits-set?)
(native any-bits-set?)
(native arithmetic-shift <int^int:int>)
(native bit-count)
(native bit-set? <int^int:bool>)
(native bitwise-and <int*:int>)
(native bitwise-ior <int*:int>)
(native bitwise-merge)
(native bitwise-not <int:int>)
(native bitwise-xor <int*:int>)
(native clear-bit-field)
(native copy-bit-field)
(native extract-bit-field <int^int^int:int>)
(native first-bit-set)
(native integer-length)
(native replace-bit-field)
(native test-bit-field?)


;;;
;;;; Number
;;;


(native finite?)
(native infinite?)
(native nan?)
(native divide-by-zero-exception-arguments)
(native divide-by-zero-exception-procedure)
(native divide-by-zero-exception?)


;;;
;;;; Parameter
;;;


(native make-parameter)


;;;
;;;; Path
;;;


(native path-directory)
(native path-expand)
(native path-extension)
(native path-normalize)
(native path-strip-directory)
(native path-strip-extension)
(native path-strip-trailing-directory-separator)
(native path-strip-volume)
(native path-volume)


;;;
;;;; Port
;;;


(native close-port)
(native console-port)
(native current-error-port)
(native port-settings-set!)
(native port?)


;; input
(native call-with-input-string)
(native call-with-input-u8vector)
(native call-with-input-vector)
(native input-port-byte-position)
(native input-port-bytes-buffered)
(native input-port-char-position)
(native input-port-characters-buffered)
(native input-port-column)
(native input-port-line)
(native input-port-readtable)
(native input-port-readtable-set!)
(native input-port-timeout-set!)
(native open-directory)
(native open-dummy)
(native open-event-queue)
(native open-file)
(native open-input-string)
(native open-input-u8vector)
(native open-input-vector)
(native open-string)
(native open-string-pipe)
(native open-u8vector)
(native open-u8vector-pipe)
(native open-vector)
(native open-vector-pipe)
(native read-all)
(native read-line)
(native read-substring)
(native read-subu8vector)
(native read-u8)
(native with-input-from-port)
(native with-input-from-string)
(native with-input-from-u8vector)
(native with-input-from-vector)
(native datum-parsing-exception-kind)
(native datum-parsing-exception-parameters)
(native datum-parsing-exception-readenv)
(native datum-parsing-exception?)
(native nonempty-input-port-character-buffer-exception-arguments)
(native nonempty-input-port-character-buffer-exception-procedure)
(native nonempty-input-port-character-buffer-exception?)


;; output
(native call-with-output-string)
(native call-with-output-u8vector)
(native call-with-output-vector)
(native force-output)
(native get-output-string)
(native get-output-u8vector <any:u8vector>)
(native get-output-vector)
(native open-output-string)
(native open-output-u8vector)
(native open-output-vector)
(native output-port-byte-position)
(native output-port-char-position)
(native output-port-column)
(native output-port-line)
(native output-port-readtable)
(native output-port-readtable-set!)
(native output-port-timeout-set!)
(native output-port-width)
(native pp)
(native pretty-print)
(native print)
(native println)
(native with-output-to-port)
(native with-output-to-string)
(native with-output-to-u8vector)
(native with-output-to-vector)
(native write-substring)
(native write-subu8vector)
(native write-u8)


;;;
;;;; Procedure
;;;


(native nonprocedure-operator-exception-arguments)
(native nonprocedure-operator-exception-code)
(native nonprocedure-operator-exception-operator)
(native nonprocedure-operator-exception-rte)
(native nonprocedure-operator-exception?)
(native number-of-arguments-limit-exception-arguments)
(native number-of-arguments-limit-exception-procedure)
(native number-of-arguments-limit-exception?)
(native unknown-keyword-argument-exception-arguments)
(native unknown-keyword-argument-exception-procedure)
(native unknown-keyword-argument-exception?)
(native wrong-number-of-arguments-exception-arguments)
(native wrong-number-of-arguments-exception-procedure)
(native wrong-number-of-arguments-exception?)


;;;
;;;; Process
;;;


(native exit)
(native main)
(native open-process)
(native process-pid)
(native process-status)
(native unterminated-process-exception-arguments)
(native unterminated-process-exception-procedure)
(native unterminated-process-exception?)


;;;
;;;; Protocol
;;;


(native protocol-info)
(native protocol-info-aliases)
(native protocol-info-name)
(native protocol-info-number)
(native protocol-info?)


;;;
;;;; Random
;;;


(native make-random-source)
(native default-random-source)
(native random-f64vector)
(native random-integer)
(native random-real <:fl>)
(native random-source-make-f64vectors)
(native random-source-make-integers)
(native random-source-make-reals)
(native random-source-make-u8vectors)
(native random-source-pseudo-randomize!)
(native random-source-randomize!)
(native random-source-state-ref)
(native random-source-state-set!)
(native random-source?)
(native random-u8vector)


;;;
;;;; Readtable
;;;


(native current-readtable)
(native readtable-case-conversion?)
(native readtable-case-conversion?-set)
(native readtable-comment-handler-set)
(native readtable-eval-allowed?)
(native readtable-eval-allowed?-set)
(native readtable-keywords-allowed?)
(native readtable-keywords-allowed?-set)
(native readtable-max-unescaped-char)
(native readtable-max-unescaped-char-set)
(native readtable-max-write-length)
(native readtable-max-write-length-set)
(native readtable-max-write-level)
(native readtable-max-write-level-set)
(native readtable-sharing-allowed?)
(native readtable-sharing-allowed?-set)
(native readtable-start-syntax)
(native readtable-start-syntax-set)
(native readtable-write-cdr-read-macros?)
(native readtable-write-cdr-read-macros?-set)
(native readtable-write-extended-read-macros?)
(native readtable-write-extended-read-macros?-set)
(native readtable?)


;;;
;;;; REPL
;;;


(native repl-display-environment?)
(native repl-input-port)
(native repl-output-port)
(native repl-result-history-max-length-set!)
(native repl-result-history-ref)


;;;
;;;; RPC
;;;


(native rpc-remote-error-exception-arguments)
(native rpc-remote-error-exception-message)
(native rpc-remote-error-exception-procedure)
(native rpc-remote-error-exception?)


;;;
;;;; Serialized
;;;


(native make-serialized)
(native serialized?)
(native serialized-class)
(native serialized-content)


;;;
;;;; Service
;;;


(native service-info)
(native service-info-aliases)
(native service-info-name)
(native service-info-port-number)
(native service-info-protocol)
(native service-info?)


;;;
;;;; Shell
;;;


(native shell-command)


;;;
;;;; String
;;;


(native append-strings)
(native object->string)
(native substring-fill!)
(native substring-move!)
(native string-shrink!)


;;;
;;;; Symbol
;;;


(native gensym)
(native make-uninterned-symbol)
(native uninterned-symbol?)


;;;
;;;; System
;;;


(native configure-command-string)
(native system-stamp)
(native system-type)
(native system-type-string)
(native system-version)
(native system-version-string)


;;;
;;;; Table
;;;


(native make-table <object*:table>)
(native table->list)
(native table-copy)
(native table-for-each)
(native table-length)
(native table-merge)
(native table-merge!)
(native table-ref)
(native table-search)
(native table-set!)
(native table?)
(native list->table)
(native unbound-table-key-exception-arguments)
(native unbound-table-key-exception-procedure)
(native unbound-table-key-exception?)


;;;
;;;; TCP
;;;


(native open-tcp-client)
(native open-tcp-server)
(native tcp-client-peer-socket-info)
(native tcp-client-self-socket-info)
(native tcp-server-socket-info)
(native tcp-service-register!)
(native tcp-service-unregister!)
(native socket-info-address)
(native socket-info-family)
(native socket-info-port-number)
(native socket-info?)


;;;
;;;; Thread
;;;


(native current-thread)
(native make-root-thread)
(native make-thread)
(native make-thread-group)
(native thread-base-priority)
(native thread-base-priority-set!)
(native thread-group->thread-group-list)
(native thread-group->thread-group-vector)
(native thread-group->thread-list)
(native thread-group->thread-vector)
(native thread-group-name)
(native thread-group-parent)
(native thread-group-resume!)
(native thread-group-suspend!)
(native thread-group-terminate!)
(native thread-group?)
(native thread-init!)
(native thread-interrupt!)
(native thread-join!)
(native thread-mailbox-extract-and-rewind)
(native thread-mailbox-next)
(native thread-mailbox-rewind)
(native thread-name)
(native thread-priority-boost)
(native thread-priority-boost-set!)
(native thread-quantum)
(native thread-quantum-set!)
(native thread-receive)
(native thread-resume!)
(native thread-send)
(native thread-sleep!)
(native thread-specific)
(native thread-specific-set!)
(native thread-start!)
(native thread-state)
(native thread-state-abnormally-terminated-reason)
(native thread-state-abnormally-terminated?)
(native thread-state-initialized?)
(native thread-state-normally-terminated-result)
(native thread-state-normally-terminated?)
(native thread-state-running?)
(native thread-state-uninitialized?)
(native thread-state-waiting?)
(native thread-state-waiting-for)
(native thread-state-waiting-timeout)
(native thread-suspend!)
(native thread-terminate!)
(native thread-thread-group)
(native thread-yield!)
(native thread?)
(native top)
(native deadlock-exception?)
(native inactive-thread-exception-arguments)
(native inactive-thread-exception-procedure)
(native inactive-thread-exception?)
(native initialized-thread-exception-arguments)
(native initialized-thread-exception-procedure)
(native initialized-thread-exception?)
(native join-timeout-exception-arguments)
(native join-timeout-exception-procedure)
(native join-timeout-exception?)
(native mailbox-receive-timeout-exception-arguments)
(native mailbox-receive-timeout-exception-procedure)
(native mailbox-receive-timeout-exception?)
(native scheduler-exception-reason)
(native scheduler-exception?)
(native started-thread-exception-arguments)
(native started-thread-exception-procedure)
(native started-thread-exception?)
(native terminated-thread-exception-arguments)
(native terminated-thread-exception-procedure)
(native terminated-thread-exception?)
(native uninitialized-thread-exception-arguments)
(native uninitialized-thread-exception-procedure)
(native uninitialized-thread-exception?)


;;;
;;;; Time
;;;


(native cpu-time <:fl>)
(native current-time)
(native process-times)
(native real-time <:fl>)
(native seconds->time)
(native time->seconds)
(native time?)
(native timeout->time)


;;;
;;;; TTY
;;;


(native tty-history)
(native tty-history-max-length-set!)
(native tty-history-set!)
(native tty-mode-set!)
(native tty-paren-balance-duration-set!)
(native tty-text-attributes-set!)
(native tty-type-set!)
(native tty?)


;;;
;;;; Type
;;;


(native improper-length-list-exception-arg-num)
(native improper-length-list-exception-arguments)
(native improper-length-list-exception-procedure)
(native improper-length-list-exception?)
(native range-exception-arg-num)
(native range-exception-arguments)
(native range-exception-procedure)
(native range-exception?)
(native type-exception-arg-num)
(native type-exception-arguments)
(native type-exception-procedure)
(native type-exception-type-id)
(native type-exception?)


;;;
;;;; UDP
;;;


(native open-udp)
(native udp-destination-set!)
(native udp-read-u8vector)
(native udp-write-u8vector)
(native udp-read-subu8vector)
(native udp-write-subu8vector)
(native udp-local-socket-info)
(native udp-source-socket-info)


;;;
;;;; User
;;;


(native user-info)
(native user-info-gid)
(native user-info-home)
(native user-info-name)
(native user-info-shell)
(native user-info-uid)
(native user-info?)
(native user-name)


;;;
;;;; Vector
;;;


(native append-vectors)
(native vector-append)
(native vector-copy)
(native vector-shrink!)
(native subvector)
(native subvector-fill!)
(native subvector-move!)


;;;
;;;; Will
;;;


(native make-will)
(native will-execute!)
(native will-testator)
(native will?))

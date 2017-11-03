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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2015
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
;;;; Homogeneous
;;;


;; s8
(native s8vector <:s8vector>)
(native make-s8vector <fx^opt<int>:s8vector>)
(native s8vector-length)
(native s8vector-ref)
(native s8vector-set!)
(native s8vector?)
(native s8vector->list)
(native list->s8vector)
(native s8vector-append)
(native s8vector-copy)
(native s8vector-fill!)
(native subs8vector)

;; u8
(native u8vector <:u8vector>)
(native make-u8vector <fx^opt<int>:u8vector>)
(native u8vector-length)
(native u8vector-ref <u8vector^fb:fx>)
(native u8vector-set!)
(native u8vector?)
(native u8vector->list)
(native list->u8vector)
(native u8vector->object)
(native object->u8vector)
(native u8vector-shrink!)
(native u8vector-append)
(native u8vector-copy)
(native u8vector-fill!)
(native subu8vector)
(native subu8vector-move!)

;; s16
(native s16vector <:s16vector>)
(native make-s16vector <fx^opt<int>:s16vector>)
(native s16vector-length)
(native s16vector-ref <s16vector^fb:fx>)
(native s16vector-set!)
(native s16vector?)
(native s16vector->list)
(native list->s16vector)
(native s16vector-append)
(native s16vector-copy)
(native s16vector-fill!)
(native subs16vector)

;; u16
(native u16vector <:u16vector>)
(native make-u16vector <fx^opt<int>:u16vector>)
(native u16vector-length)
(native u16vector-ref <u16vector^fb:fx>)
(native u16vector-set!)
(native u16vector?)
(native u16vector->list)
(native u16vector-fill!)
(native list->u16vector)
(native u16vector-append)
(native u16vector-copy)
(native u16vector-fill!)
(native subu16vector)

;; s32
(native s32vector <:s32vector>)
(native make-s32vector <fx^opt<int>:s32vector>)
(native s32vector-length)
(native s32vector-ref <s32vector^fb:fx>)
(native s32vector-set!)
(native s32vector?)
(native s32vector->list)
(native list->s32vector)
(native s32vector-append)
(native s32vector-copy)
(native s32vector-fill!)
(native subs32vector-move!)
(native subs32vector)

;; u32
(native u32vector <:u32vector>)
(native make-u32vector <fx^opt<int>:u32vector>)
(native u32vector-length)
(native u32vector-ref <u32vector^fb:fx>)
(native u32vector-set!)
(native u32vector?)
(native u32vector->list)
(native list->u32vector)
(native u32vector-append)
(native u32vector-copy)
(native u32vector-fill!)
(native subu32vector)

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


;;conflict;;(native default-random-source)
(native abandoned-mutex-exception?)
(native abort)
(native all-bits-set?)
(native any-bits-set?)
(native arithmetic-shift)
(native bit-count)
(native bit-set?)
(native bitwise-and)
(native bitwise-ior)
(native bitwise-merge)
(native bitwise-not)
(native bitwise-xor)
(native box)
(native box?)
;;conflict;;(native break)
(native call-with-input-string)
(native call-with-input-u8vector)
(native call-with-input-vector)
(native call-with-output-string)
(native call-with-output-u8vector)
(native call-with-output-vector)
(native call/cc)
(native cfun-conversion-exception-arguments)
(native cfun-conversion-exception-code)
(native cfun-conversion-exception-message)
(native cfun-conversion-exception-procedure)
(native cfun-conversion-exception?)
(native clear-bit-field)
;;conflict;;(native close-port)
(native command-line)
(native compile-file)
(native compile-file-to-target)
(native condition-variable-broadcast!)
(native condition-variable-name)
(native condition-variable-signal!)
(native condition-variable-specific)
(native condition-variable-specific-set!)
(native condition-variable?)
(native console-port)
;;conflict;;(native continuation-capture)
;;conflict;;(native continuation-graft)
;;conflict;;(native continuation-return)
;;conflict;;(native continuation?)
(native copy-bit-field)
;;conflict;;(native copy-file)
;;conflict;;(native cpu-time)
(native create-directory)
(native create-fifo)
(native create-link)
(native create-symbolic-link)
;;conflict;;(native current-directory)
(native current-error-port)
;;conflict;;(native current-exception-handler)
(native current-readtable)
;;conflict;;(native current-thread)
;;conflict;;(native current-time)
(native current-user-interrupt-handler)
(native datum-parsing-exception-kind)
(native datum-parsing-exception-parameters)
(native datum-parsing-exception-readenv)
(native datum-parsing-exception?)
(native deadlock-exception?)
(native defer-user-interrupts)
;;conflict;;(native delete-directory)
;;conflict;;(native delete-file)
(native directory-files)
;;conflict;;(native display-continuation-backtrace)
(native display-continuation-dynamic-environment)
(native display-continuation-environment)
(native display-dynamic-environment?)
(native display-environment-set!)
;;conflict;;(native display-exception)
(native display-exception-in-context)
(native display-procedure-environment)
(native divide-by-zero-exception-arguments)
(native divide-by-zero-exception-procedure)
(native divide-by-zero-exception?)
;;conflict;;(native eq?-hash)
;;conflict;;(native equal?-hash)
;;conflict;;(native eqv?-hash)
(native err-code->string)
;;conflict;;(native error)
(native error-exception-message)
(native error-exception-parameters)
(native error-exception?)
;;conflict;;(native exit)
(native expression-parsing-exception-kind)
(native expression-parsing-exception-parameters)
(native expression-parsing-exception-source)
(native expression-parsing-exception?)
(native extract-bit-field)
(native file-attributes)
(native file-creation-time)
(native file-device)
;;conflict;;(native file-exists?)
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
(native file-mode)
(native file-number-of-links)
(native file-owner)
(native file-size)
(native file-type)
(native finite?)
(native first-bit-set)
;;conflict;;(native fixnum->flonum)
(native fixnum-overflow-exception-arguments)
(native fixnum-overflow-exception-procedure)
(native fixnum-overflow-exception?)
(native fixnum?)
;;conflict;;(native fl*)
;;conflict;;(native fl+)
;;conflict;;(native fl-)
;;conflict;;(native fl/)
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
(native flonum?)
(native flpositive?)
(native flround)
(native flsin)
(native flsqrt)
(native fltan)
(native fltruncate)
(native flzero?)
(native force-output)
;;conflict;;(native foreign-address)
;;conflict;;(native foreign-release!)
;;conflict;;(native foreign-released?)
;;conflict;;(native foreign-tags)
;;conflict;;(native foreign?)
(native fx*)
(native fx+)
(native fx-)
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
;;conflict;;(native gc-report-set!)
(native generate-proper-tail-calls)
(native gensym)
(native get-output-string)
(native get-output-u8vector)
(native get-output-vector)
(native getenv)
(native group-info)
(native group-info-gid)
(native group-info-members)
(native group-info-name)
(native group-info?)
(native heap-overflow-exception?)
(native host-info)
(native host-info-addresses)
(native host-info-aliases)
(native host-info-name)
(native host-info?)
(native host-name)
(native improper-length-list-exception-arg-num)
(native improper-length-list-exception-arguments)
(native improper-length-list-exception-procedure)
(native improper-length-list-exception?)
(native inactive-thread-exception-arguments)
(native inactive-thread-exception-procedure)
(native inactive-thread-exception?)
(native infinite?)
(native initialized-thread-exception-arguments)
(native initialized-thread-exception-procedure)
(native initialized-thread-exception?)
(native input-port-byte-position)
(native input-port-bytes-buffered)
(native input-port-char-position)
(native input-port-characters-buffered)
(native input-port-column)
(native input-port-line)
(native input-port-readtable)
(native input-port-readtable-set!)
;;conflict;;(native input-port-timeout-set!)
(native integer-length)
(native integer-nth-root)
(native integer-sqrt)
(native invalid-hash-number-exception-arguments)
(native invalid-hash-number-exception-procedure)
(native invalid-hash-number-exception?)
(native join-timeout-exception-arguments)
(native join-timeout-exception-procedure)
(native join-timeout-exception?)
;;conflict;;(native keyword->string)
(native keyword-expected-exception-arguments)
(native keyword-expected-exception-procedure)
(native keyword-expected-exception?)
(native keyword-hash)
;;conflict;;(native keyword?)
(native link-flat)
(native link-incremental)
(native list->table)
(native mailbox-receive-timeout-exception-arguments)
(native mailbox-receive-timeout-exception-procedure)
(native mailbox-receive-timeout-exception?)
(native main)
(native make-condition-variable)
;;conflict;;(native make-mutex)
(native make-parameter)
(native make-random-source)
;;conflict;;(native make-root-thread)
(native make-table)
;;conflict;;(native make-thread)
;;conflict;;(native make-thread-group)
(native make-uninterned-keyword)
(native make-uninterned-symbol)
(native make-will)
(native multiple-c-return-exception?)
;;conflict;;(native mutex-lock!)
;;conflict;;(native mutex-name)
;;conflict;;(native mutex-specific)
;;conflict;;(native mutex-specific-set!)
;;conflict;;(native mutex-state)
;;conflict;;(native mutex-unlock!)
;;conflict;;(native mutex?)
(native nan?)
(native network-info)
(native network-info-aliases)
(native network-info-name)
(native network-info-number)
(native network-info?)
(native no-such-file-or-directory-exception-arguments)
(native no-such-file-or-directory-exception-procedure)
(native no-such-file-or-directory-exception?)
(native noncontinuable-exception-reason)
(native noncontinuable-exception?)
(native nonempty-input-port-character-buffer-exception-arguments)
(native nonempty-input-port-character-buffer-exception-procedure)
(native nonempty-input-port-character-buffer-exception?)
(native nonprocedure-operator-exception-arguments)
(native nonprocedure-operator-exception-code)
(native nonprocedure-operator-exception-operator)
(native nonprocedure-operator-exception-rte)
(native nonprocedure-operator-exception?)
(native number-of-arguments-limit-exception-arguments)
(native number-of-arguments-limit-exception-procedure)
(native number-of-arguments-limit-exception?)
(native object->serial-number)
(native object->string)
(native object->u8vector)
(native open-directory)
(native open-dummy)
(native open-event-queue)
(native open-file)
(native open-input-string)
(native open-input-u8vector)
(native open-input-vector)
(native open-output-string)
(native open-output-u8vector)
(native open-output-vector)
;;conflict;;(native open-process)
(native open-string)
(native open-string-pipe)
;;conflict;;(native open-tcp-client)
;;conflict;;(native open-tcp-server)
(native open-u8vector)
(native open-u8vector-pipe)
(native open-vector)
(native open-vector-pipe)
(native os-exception-arguments)
(native os-exception-code)
(native os-exception-message)
(native os-exception-procedure)
(native os-exception?)
(native output-port-byte-position)
(native output-port-char-position)
(native output-port-column)
(native output-port-line)
(native output-port-readtable)
(native output-port-readtable-set!)
;;conflict;;(native output-port-timeout-set!)
(native output-port-width)
(native path-directory)
(native path-expand)
(native path-extension)
(native path-normalize)
(native path-strip-directory)
(native path-strip-extension)
(native path-strip-trailing-directory-separator)
(native path-strip-volume)
(native path-volume)
(native port-settings-set!)
(native port?)
;;conflict;;(native pp)
(native pretty-print)
(native primordial-exception-handler)
;;conflict;;(native print)
(native println)
(native process-pid)
;;conflict;;(native process-status)
;;conflict;;(native process-times)
(native protocol-info)
(native protocol-info-aliases)
(native protocol-info-name)
(native protocol-info-number)
(native protocol-info?)
;;conflict;;(native raise)
(native random-f64vector)
;;conflict;;(native random-integer)
;;conflict;;(native random-real)
(native random-source-make-f64vectors)
(native random-source-make-integers)
(native random-source-make-reals)
(native random-source-make-u8vectors)
;;conflict;;(native random-source-pseudo-randomize!)
;;conflict;;(native random-source-randomize!)
(native random-source-state-ref)
(native random-source-state-set!)
(native random-source?)
(native random-u8vector)
(native range-exception-arg-num)
(native range-exception-arguments)
(native range-exception-procedure)
(native range-exception?)
;;conflict;;(native read-all)
(native read-line)
(native read-substring)
;;conflict;;(native read-subu8vector)
;;conflict;;(native read-u8)
(native readtable-case-conversion?)
(native readtable-case-conversion?-set)
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
;;conflict;;(native readtable?)
;;conflict;;(native real-time)
(native rename-file)
(native repl-display-environment?)
(native repl-input-port)
(native repl-output-port)
(native repl-result-history-max-length-set!)
(native repl-result-history-ref)
(native replace-bit-field)
(native rpc-remote-error-exception-arguments)
(native rpc-remote-error-exception-message)
(native rpc-remote-error-exception-procedure)
(native rpc-remote-error-exception?)
(native scheduler-exception-reason)
(native scheduler-exception?)
;;conflict;;(native seconds->time)
(native serial-number->object)
(native service-info)
(native service-info-aliases)
(native service-info-name)
(native service-info-port-number)
(native service-info-protocol)
(native service-info?)
(native set-box!)
(native setenv)
(native sfun-conversion-exception-arguments)
(native sfun-conversion-exception-code)
(native sfun-conversion-exception-message)
(native sfun-conversion-exception-procedure)
(native sfun-conversion-exception?)
(native shell-command)
;;conflict;;(native socket-info-address)
(native socket-info-family)
;;conflict;;(native socket-info-port-number)
(native socket-info?)
(native stack-overflow-exception?)
(native started-thread-exception-arguments)
(native started-thread-exception-procedure)
(native started-thread-exception?)
(native step)
(native step-level-set!)
;;conflict;;(native string->keyword)
;;conflict;;(native string-ci=?-hash)
;;conflict;;(native string=?-hash)
(native subvector)
(native symbol-hash)
(native system-stamp)
(native system-type)
(native system-type-string)
(native system-version)
(native system-version-string)
(native table->list)
(native table-copy)
(native table-for-each)
;;conflict;;(native table-length)
(native table-merge)
(native table-merge!)
(native table-ref)
(native table-search)
(native table-set!)
(native table?)
;;conflict;;(native tcp-client-peer-socket-info)
;;conflict;;(native tcp-client-self-socket-info)
;;conflict;;(native tcp-server-socket-info)
(native tcp-service-register!)
(native tcp-service-unregister!)
(native terminated-thread-exception-arguments)
(native terminated-thread-exception-procedure)
(native terminated-thread-exception?)
(native test-bit-field?)
;;conflict;;(native thread-base-priority)
;;conflict;;(native thread-base-priority-set!)
;;conflict;;(native thread-group->thread-group-list)
;;conflict;;(native thread-group->thread-group-vector)
;;conflict;;(native thread-group->thread-list)
;;conflict;;(native thread-group->thread-vector)
(native thread-group-name)
(native thread-group-parent)
(native thread-group-resume!)
(native thread-group-suspend!)
(native thread-group-terminate!)
(native thread-group?)
(native thread-init!)
;;conflict;;(native thread-interrupt!)
;;conflict;;(native thread-join!)
;;conflict;;(native thread-mailbox-extract-and-rewind)
;;conflict;;(native thread-mailbox-next)
;;conflict;;(native thread-mailbox-rewind)
;;conflict;;(native thread-name)
;;conflict;;(native thread-priority-boost)
;;conflict;;(native thread-priority-boost-set!)
;;conflict;;(native thread-quantum)
;;conflict;;(native thread-quantum-set!)
;;conflict;;(native thread-receive)
(native thread-resume!)
;;conflict;;(native thread-send)
;;conflict;;(native thread-sleep!)
;;conflict;;(native thread-specific)
;;conflict;;(native thread-specific-set!)
;;conflict;;(native thread-start!)
;;conflict;;(native thread-state)
;;conflict;;(native thread-state-abnormally-terminated-reason)
;;conflict;;(native thread-state-abnormally-terminated?)
;;conflict;;(native thread-state-active-timeout)
;;conflict;;(native thread-state-active-waiting-for)
;;conflict;;(native thread-state-active?)
;;conflict;;(native thread-state-initialized?)
;;conflict;;(native thread-state-normally-terminated-result)
;;conflict;;(native thread-state-normally-terminated?)
;;conflict;;(native thread-state-uninitialized?)
(native thread-suspend!)
;;conflict;;(native thread-terminate!)
;;conflict;;(native thread-thread-group)
;;conflict;;(native thread-yield!)
;;conflict;;(native thread?)
;;conflict;;(native time->seconds)
(native time?)
(native timeout->time)
(native top)
;;conflict;;(native touch)
;;conflict;;(native trace)
(native tty-history)
(native tty-history-max-length-set!)
(native tty-history-set!)
(native tty-mode-set!)
(native tty-paren-balance-duration-set!)
(native tty-text-attributes-set!)
(native tty-type-set!)
(native tty?)
(native type-exception-arg-num)
(native type-exception-arguments)
(native type-exception-procedure)
(native type-exception-type-id)
(native type-exception?)
(native unbound-global-exception-code)
(native unbound-global-exception-rte)
(native unbound-global-exception-variable)
(native unbound-global-exception?)
(native unbound-os-environment-variable-exception-arguments)
(native unbound-os-environment-variable-exception-procedure)
(native unbound-os-environment-variable-exception?)
(native unbound-serial-number-exception-arguments)
(native unbound-serial-number-exception-procedure)
(native unbound-serial-number-exception?)
(native unbound-table-key-exception-arguments)
(native unbound-table-key-exception-procedure)
(native unbound-table-key-exception?)
(native unbox)
(native unbreak)
(native uncaught-exception-arguments)
(native uncaught-exception-procedure)
(native uncaught-exception-reason)
(native uncaught-exception?)
(native uninitialized-thread-exception-arguments)
(native uninitialized-thread-exception-procedure)
(native uninitialized-thread-exception?)
(native uninterned-keyword?)
(native uninterned-symbol?)
(native unknown-keyword-argument-exception-arguments)
(native unknown-keyword-argument-exception-procedure)
(native unknown-keyword-argument-exception?)
(native unterminated-process-exception-arguments)
(native unterminated-process-exception-procedure)
(native unterminated-process-exception?)
;;conflict;;(native untrace)
(native user-info)
(native user-info-gid)
(native user-info-home)
(native user-info-name)
(native user-info-shell)
(native user-info-uid)
(native user-info?)
(native user-name)
(native vector-append)
;;conflict;;(native vector-copy)
;;conflict;;(native void)
(native will-execute!)
(native will-testator)
(native will?)
;;conflict;;(native with-exception-catcher)
;;conflict;;(native with-exception-handler)
(native with-input-from-port)
(native with-input-from-string)
(native with-input-from-u8vector)
(native with-input-from-vector)
(native with-output-to-port)
(native with-output-to-string)
(native with-output-to-u8vector)
(native with-output-to-vector)
(native write-substring)
;;conflict;;(native write-subu8vector)
;;conflict;;(native write-u8)
(native wrong-number-of-arguments-exception-arguments)
(native wrong-number-of-arguments-exception-procedure)
(native wrong-number-of-arguments-exception?))

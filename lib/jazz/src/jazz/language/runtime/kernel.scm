;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Language Kernel
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


(module protected jazz.language.runtime.kernel scheme


;;;
;;;; Backend
;;;


(native jazz:new-backend)
(native jazz:register-backend)
(native jazz:register-emit)
(native jazz:add-backend-binding)
(native jazz:emit-namespace-statements)
(native jazz:new-code)
(native jazz:sourcified-form)
(native jazz:sourcified-form2)
(native jazz:codes-forms)


;;;
;;;; Boolean
;;;


(native jazz:boolean)


;;;
;;;; Box
;;;


(native box?)
(native box)
(native unbox)
(native set-box!)


;;;
;;;; Call-Site
;;;


(native jazz:new-call-site)


;;;
;;;; Category
;;;


(native jazz:get-category-identifier)
(native jazz:get-category-descendants)
(native jazz:get-class-ascendant)


;;;
;;;; Char
;;;


(native jazz:symbolic-char)
(native jazz:char-symbol)


;;;
;;;; Closure
;;;


(native jazz:closure?)
(native jazz:closure-code)
(native jazz:closure-length)
(native jazz:closure-ref)


;;;
;;;; Continuation
;;;


(native jazz:continuation?)
(native jazz:continuation-capture)
(native jazz:continuation-graft)
(native jazz:continuation-graft-no-winding)
(native jazz:continuation-return)
(native jazz:continuation-checkpoint)
(native jazz:continuation-parent)
(native jazz:continuation-creator)
(native jazz:continuation-locat)
(native jazz:continuation-next)


;;;
;;;; Core
;;;


(native jazz:compile-unit)
(native jazz:build-unit)
(native jazz:build-image)
(native jazz:for-each-subunit)
(native jazz:load-manifest)


;;;
;;;; Debug
;;;


(native jazz:run-loop?)
(native jazz:terminal)
(native jazz:terminal-string)
(native jazz:terminal-line)
(native jazz:terminal-port)
(native jazz:unimplemented)
(native jazz:dump-stack)
(native jazz:log-object)
(native jazz:log-string)
(native jazz:log-newline)
(native jazz:close-log)
(native jazz:inspect-object)
(native jazz:object->vector)
(native jazz:vector->object)
(native jazz:object-copy)
(native generate-proper-tail-calls)
(native step)
(native step-level-set!)
(native install-step-handler)


;;;
;;;; Development
;;;


;; These globals should really be some readtime mecanism like Gambit's #
;; Need to discuss with Marc Feeley about how to do that cleanly


(native ?) (native get-?) (native set-?)
(native %) (native get-%) (native set-%)


(native ?a) (native get-?a) (native set-?a)
(native ?b) (native get-?b) (native set-?b)
(native ?c) (native get-?c) (native set-?c)
(native ?d) (native get-?d) (native set-?d)
(native ?e) (native get-?e) (native set-?e)
(native ?f) (native get-?f) (native set-?f)
(native ?g) (native get-?g) (native set-?g)
(native ?h) (native get-?h) (native set-?h)
(native ?i) (native get-?i) (native set-?i)
(native ?j) (native get-?j) (native set-?j)
(native ?k) (native get-?k) (native set-?k)
(native ?l) (native get-?l) (native set-?l)
(native ?m) (native get-?m) (native set-?m)
(native ?n) (native get-?n) (native set-?n)
(native ?o) (native get-?o) (native set-?o)
(native ?p) (native get-?p) (native set-?p)
(native ?q) (native get-?q) (native set-?q)
(native ?r) (native get-?r) (native set-?r)
(native ?s) (native get-?s) (native set-?s)
(native ?t) (native get-?t) (native set-?t)
(native ?u) (native get-?u) (native set-?u)
(native ?v) (native get-?v) (native set-?v)
(native ?w) (native get-?w) (native set-?w)
(native ?x) (native get-?x) (native set-?x)
(native ?y) (native get-?y) (native set-?y)
(native ?z) (native get-?z) (native set-?z)


(native %a) (native get-%a) (native set-%a)
(native %b) (native get-%b) (native set-%b)
(native %c) (native get-%c) (native set-%c)
(native %d) (native get-%d) (native set-%d)
(native %e) (native get-%e) (native set-%e)
(native %f) (native get-%f) (native set-%f)
(native %g) (native get-%g) (native set-%g)
(native %h) (native get-%h) (native set-%h)
(native %i) (native get-%i) (native set-%i)
(native %j) (native get-%j) (native set-%j)
(native %k) (native get-%k) (native set-%k)
(native %l) (native get-%l) (native set-%l)
(native %m) (native get-%m) (native set-%m)
(native %n) (native get-%n) (native set-%n)
(native %o) (native get-%o) (native set-%o)
(native %p) (native get-%p) (native set-%p)
(native %q) (native get-%q) (native set-%q)
(native %r) (native get-%r) (native set-%r)
(native %s) (native get-%s) (native set-%s)
(native %t) (native get-%t) (native set-%t)
(native %u) (native get-%u) (native set-%u)
(native %v) (native get-%v) (native set-%v)
(native %w) (native get-%w) (native set-%w)
(native %x) (native get-%x) (native set-%x)
(native %y) (native get-%y) (native set-%y)
(native %z) (native get-%z) (native set-%z)


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


(native jazz:enumerator?)
(native jazz:enumerator->symbol)
(native jazz:symbol->enumerator)

;;;
;;;; Environment
;;;


(native getenv)
(native setenv)


;;;
;;;; Exception
;;;


(native jazz:exception-reason)
(native jazz:exception-detail)
(native jazz:exception-location)
(native jazz:display-exception)
(native display-exception-in-context)
(native jazz:display-continuation-backtrace)
(native jazz:get-exception-hook)
(native jazz:set-exception-hook)
(native jazz:invoke-exception-hook)
(native jazz:system-exception-hook)
(native jazz:current-exception-handler)
(native jazz:with-exception-handler)
(native jazz:with-exception-catcher)
(native jazz:with-exception-filter)
(native jazz:with-exception-propagater)
(native jazz:dump-exception)
(native jazz:raise)


;;;
;;;; Exception-Detail
;;;


(native jazz:new-exception-detail)


;;;
;;;; Field
;;;


(native jazz:field? <object:bool>)
(native jazz:field-name)
(native jazz:category-field)
(native jazz:add-field)


;;;
;;;; Fifo
;;;


(native fifo->list)


;;;
;;;; Fixnum
;;;


(native fixnum? <object:bool>)
(native flonum? <object:bool>)
(native jazz:fixnum->flonum <fx:fl>)
(native jazz:flonum->fixnum <fl:fx>)
(native fx+ <fx^fx:fx>)
(native fx- <fx^fx:fx>)
(native fx* <fx^fx:fx>)
(native jazz:+infinity)
(native jazz:-infinity)
(native fxmodulo)
(native fxquotient)
(native fxand)


;;;
;;;; Float
;;;


(native finite?)
(native infinite?)
(native nan?)


;;;
;;;; Flonum
;;;


;; until proper call site casting of native calls

(native undocumented jazz:sharp/sharp/fl+ <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl- <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl* <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl/ <fl^fl:fl>)

(native flalloc)
(native flset!)


;;;
;;;; Foreign
;;;


(native jazz:foreign?)
(native jazz:foreign-address)
(native jazz:foreign-release!)
(native jazz:foreign-released?)
(native jazz:foreign-tags)
;;(native jazz:still-obj-refcount)
(native jazz:still-obj-refcount-dec!)
(native jazz:still-obj-refcount-inc!)
(native jazz:still-copy)


;;;
;;;; Gambit
;;;


(native base64-string->u8vector)
(native base64-substring->u8vector)
(native compile-file)
(native compile-file-to-target)
(native datum-parsing-exception?)
(native inactive-thread-exception?)
(native fxwrap*)
(native fxwrap+)
(native fxwrap-)
(native subu8vector->base64-string)
(native u8vector->base64-string)
(native object->string)
(native os-exception-arguments)
(native os-exception-code)
(native os-exception-message)
(native os-exception-procedure)
(native os-exception?)
(native system-version-string)


;;;
;;;; Heartbeat
;;;


(native jazz:jazz-heartbeat)


;;;
;;;; Homogeneous
;;;


;; s8
(native s8vector)
(native make-s8vector <fx^opt<int>:s8vector>)
(native s8vector-length)
(native s8vector-ref)
(native s8vector-set!)
(native s8vector?)
(native s8vector->list)
(native list->s8vector)

;; u8
(native u8vector)
(native make-u8vector <fx^opt<int>:u8vector>)
(native u8vector-length)
(native u8vector-ref)
(native u8vector-set!)
(native u8vector?)
(native u8vector->list)
(native list->u8vector)
(native u8vector->object)
(native object->u8vector)
(native u8vector-shrink!)
(native append-u8vectors)
(native u8vector-append)
(native u8vector-fill!)
(native subu8vector)

;; s16
(native s16vector)
(native make-s16vector <fx^opt<int>:s16vector>)
(native s16vector-length)
(native s16vector-ref)
(native s16vector-set!)
(native s16vector?)
(native s16vector->list)
(native list->s16vector)

;; u16
(native u16vector)
(native make-u16vector <fx^opt<int>:u16vector>)
(native u16vector-length)
(native u16vector-ref)
(native u16vector-set!)
(native u16vector?)
(native u16vector->list)
(native u16vector-fill!)
(native list->u16vector)

;; s32
(native s32vector)
(native make-s32vector <fx^opt<int>:s32vector>)
(native s32vector-length)
(native s32vector-ref)
(native s32vector-set!)
(native s32vector?)
(native s32vector->list)
(native list->s32vector)

;; u32
(native u32vector)
(native make-u32vector <fx^opt<int>:u32vector>)
(native u32vector-length)
(native u32vector-ref)
(native u32vector-set!)
(native u32vector?)
(native u32vector->list)
(native list->u32vector)

;; s64
(native s64vector)
(native make-s64vector <fx^opt<int>:s64vector>)
(native s64vector-length)
(native s64vector-ref)
(native s64vector-set!)
(native s64vector?)
(native s64vector->list)
(native list->s64vector)

;; u64
(native u64vector)
(native make-u64vector <fx^opt<int>:u64vector>)
(native u64vector-length)
(native u64vector-ref)
(native u64vector-set!)
(native u64vector?)
(native u64vector->list)
(native list->u64vector)

;; f32
(native f32vector)
(native make-f32vector <fx^opt<fl>:f32vector>)
(native f32vector-length)
(native f32vector-ref)
(native f32vector-set!)
(native f32vector?)
(native f32vector->list)
(native list->f32vector)
(native f32vector-copy)
(native subf32vector)
(native subf32vector-fill!)
(native subf32vector-move!)

;; f64
(native f64vector)
(native make-f64vector <fx^opt<fl>:f64vector>)
(native f64vector-length)
(native f64vector-ref)
(native f64vector-set!)
(native f64vector?)
(native f64vector->list)
(native list->f64vector)
(native f64vector-copy)
(native subf64vector)
(native subf64vector-fill!)
(native subf64vector-move!)


;;;
;;;; Host
;;;


(native command-line)
(native jazz:command-executable)
(native jazz:command-arguments)
(native user-name)
(native host-name)


;;;
;;;; Identifier
;;;


(native jazz:composite-identifier?)
(native jazz:composite-reference?)
(native jazz:compose-identifier)
(native jazz:split-symbol)
(native jazz:split-identifier)
(native jazz:split-reference)
(native jazz:break-reference)
(native jazz:identifier-name <symbol>)
(native jazz:reference-unit <symbol>)
(native jazz:reference-name <symbol>)


;;;
;;;; Instance
;;;


;;(native jazz:current-instance)


;;;
;;;; Integer
;;;


(native bitwise-not <int:int>)
(native bitwise-and <int*:int>)
(native bitwise-ior <int*:int>)
(native bitwise-xor <int*:int>)
(native arithmetic-shift <int^int:int>)
(native jazz:arithmetic-shift-left <int^int:int>)
(native jazz:arithmetic-shift-right <int^int:int>)
(native bit-set? <int^int:bool>)
(native extract-bit-field <int^int^int:int>)


;;;
;;;; Kernel
;;;


(native jazz:build-feedback)
(native jazz:boot-directory)
(native jazz:kernel-system)
(native jazz:kernel-platform)
(native jazz:kernel-windowing)
(native jazz:kernel-safety)
(native jazz:kernel-optimize?)
(native jazz:kernel-debug-environments?)
(native jazz:kernel-debug-location?)
(native jazz:kernel-debug-source?)
(native jazz:kernel-debug-foreign?)
(native jazz:kernel-mutable-bindings?)
(native jazz:kernel-destination)
(native jazz:kernel-properties)
(native jazz:kernel-built)
(native jazz:kernel-path)
(native jazz:kernel-install)
(native jazz:kernel-bundle-install)
(native jazz:kernel-bundle-root)
(native jazz:kernel-source)
(native jazz:kernel-source-accessible?)
(native jazz:kernel-source-access?)
(native jazz:kernel-version)
(native jazz:install-path)
(native jazz:bundle-depth)
(native jazz:get-update-version)
(native jazz:get-update-targets)
(native jazz:get-update-description)
(native jazz:get-jazz-version-number)
(native jazz:jazz-settings-directory)
(native jazz:jazz-settings-version)
(native jazz:list->updates)
(native jazz:versioned-directory)
(native jazz:path->container-override)
(native jazz:platform-eol-encoding)
(native jazz:load-debuggee-units)
(native jazz:jazz-product)
(native jazz:jazz-profile)
(native jazz:load-verbose?)
(native jazz:outline-verbose?)
(native jazz:done-verbose?)
(native jazz:warn-interpreted?)
(native jazz:force-interpreted?)
(native jazz:delay-reporting?)
(native jazz:use-print?)
(native jazz:use-debugger?)
(native jazz:use-snapshot?)
(native jazz:font-engine)
(native jazz:compile-options)
(native jazz:get-repositories)
(native jazz:get-build-repository)
(native jazz:load-repository)
(native jazz:make-repository)
(native jazz:install-repository)
(native jazz:uninstall-repository)
(native jazz:require-repository)
(native jazz:find-repository)
(native jazz:find-package)
(native jazz:repository?)
(native jazz:repository-name)
(native jazz:repository-title)
(native jazz:repository-directory)
(native jazz:repository-library-root)
(native jazz:repository-library-directory)
(native jazz:repository-binary?)
(native jazz:repository-dependencies)
(native jazz:repository-packages)
(native jazz:repository-find-package)
(native jazz:repository-install-packages)
(native jazz:repository-add-package)
(native jazz:repository-remove-package)
(native jazz:repository-pathname)
(native jazz:package?)
(native jazz:package-repository)
(native jazz:package-name)
(native jazz:package-root)
(native jazz:package-pathname)
(native jazz:package-root-pathname)
(native jazz:package-directory)
(native jazz:package-products)
(native jazz:package-profiles)
(native jazz:package-profiles-set!)
(native jazz:package-project)
(native jazz:split-version)
(native jazz:present-version)
(native jazz:load-package)
(native jazz:register-product)
(native jazz:register-product-run)
(native jazz:get-product-descriptor)
(native jazz:find-product-descriptor)
(native jazz:product-descriptor-name)
(native jazz:product-descriptor-unit)
(native jazz:product-descriptor-title)
(native jazz:product-descriptor-icon)
(native jazz:product-descriptor-run)
(native jazz:product-descriptor-update)
(native jazz:product-descriptor-build)
(native jazz:product-descriptor-build-bundle)
(native jazz:product-descriptor-dependencies)
(native jazz:cond-expanded-product-descriptor-update)
(native jazz:cond-expanded-product-descriptor-dependencies)
(native jazz:run-product-descriptor)
(native jazz:update-product-descriptor)
(native jazz:build-product-descriptor)
(native jazz:custom-compile/build)
(native jazz:current-process-product)
(native jazz:current-process-name)
(native jazz:current-process-name-set!)
(native jazz:current-process-title)
(native jazz:current-process-title-set!)
(native jazz:current-process-traits)
(native jazz:current-process-traits-set!)
(native jazz:current-process-icon)
(native jazz:current-process-icon-set!)
(native jazz:current-process-version)
(native jazz:current-process-version-set!)
(native jazz:current-process-present)
(native jazz:destination-directory)
(native jazz:executable-extension)
(native jazz:quote-pathname)
(native jazz:quote-jazz-pathname)
(native jazz:run-product)
(native jazz:update-product)
(native jazz:build-product)
(native jazz:install-product)
(native jazz:descendant-unit?)
(native jazz:find-pathname-unit)
(native jazz:find-unit-src)
(native jazz:gather-profiles)
(native jazz:make-profile)
(native jazz:profile-name)
(native jazz:profile-title)
(native jazz:profile-appl)
(native jazz:profile-unit)
(native jazz:get-environment)
(native jazz:get-environment-unit)
(native jazz:unit-loaded?)
(native jazz:unit-status)
(native jazz:unit-uptodate-binary?)
(native jazz:load-unit)
(native jazz:load-hook)
(native jazz:unload-unit)
(native jazz:reload-unit)
(native jazz:load-file)
(native jazz:load-script)
(native jazz:get-load-script-hook)
(native jazz:set-load-script-hook)
(native jazz:current-script-arguments)
(native jazz:get-foreign-libraries)
(native jazz:register-foreign-libraries)
(native jazz:registered-foreign-libraries)
(native jazz:get-load-mutex)
(native jazz:get-load-thread)
(native jazz:current-load-stack)
(native jazz:get-modules)
(native jazz:get-module)
(native jazz:require-module)
(native jazz:module-get)
(native jazz:module-ref)
(native jazz:module-set!)
(native jazz:register-reader-extension)
(native jazz:get-load-interpreted-hook)
(native jazz:set-load-interpreted-hook)
(native jazz:get-evaluate-forms-hook)
(native jazz:set-evaluate-forms-hook)
(native jazz:get-console-evaluate-hook)
(native jazz:set-console-evaluate-hook)
(native jazz:walk-literal)
(native jazz:walk-for)
(native jazz:register-service)
(native jazz:get-service)
(native jazz:require-service)
(native jazz:register-distributed-service)
(native jazz:get-distributed-service)
(native jazz:activate-distributed-service)
(native jazz:global-bound?)
(native jazz:global-ref)
(native jazz:global-set!)
(native jazz:global-unbind!)
(native jazz:testing?)
(native jazz:testing)
(native jazz:generate-symbol-for)
(native jazz:generate-symbol-context)
(native jazz:generate-symbol-counter)
(native jazz:image-load-counter)
(native jazz:object-load-counter)
(native jazz:interpreted-load-counter)
(native jazz:compiler-present?)
(native jazz:add-exit-job!)
(native jazz:log-backtrace)
(native jazz:set-crash-reporter)
(native jazz:crash-process)
(native jazz:invoke-process)
(native jazz:call-process)
(native jazz:feedback)
(native jazz:unit-loadable?)
(native jazz:load-foundation)
(native jazz:load-runtime)
(native jazz:load-build)
(native jazz:split-command-line)
(native jazz:get-option)
(native main)


;;;
;;;; Keyword
;;;


(native jazz:keyword?)
(native jazz:string->keyword)
(native jazz:keyword->string)


;;;
;;;; List
;;;


(native jazz:not-null?)
(native jazz:listify)
(native jazz:list-copy)
(native jazz:last-pair)
(native jazz:proper-list)


;;;
;;;; Logging
;;;


(native jazz:logging?)
(native jazz:set-logging?)


;;;
;;;; Memory
;;;


(native jazz:gc)
(native jazz:gc-count)
(native jazz:gc-statistics)
(native jazz:gc-report-set!)
(native jazz:add-gc-interrupt-job!)
(native jazz:last-gc-real-time)
(native jazz:get-min-heap)
(native jazz:set-min-heap!)
(native jazz:get-max-heap)
(native jazz:set-max-heap!)
(native jazz:process-memory)
(native jazz:symbols-memory)
(native jazz:symbol-table)
(native jazz:keyword-table)
(native jazz:bytes-allocated!)
(native jazz:classes-statistics)
(native jazz:vector-size)
(native jazz:f64vector-size)
(native jazz:list-size)
(native jazz:table-size)
(native jazz:get-live-percent)
(native jazz:raise-heap-overflow-exception)
(native jazz:get-heap-pointer)


;;;
;;;; Mutation
;;;


(native jazz:loading-module)
(native jazz:get-mutations)
(native jazz:reset-mutations)
(native jazz:register-mutation)


;;;
;;;; Network
;;;


(native jazz:open-tcp-client)
(native jazz:open-tcp-server)
(native jazz:tcp-server-socket-info)
(native jazz:call-with-tcp-client)


;;;
;;;; Object
;;;


(native jazz:new)
(native jazz:nascent-new)
(native jazz:class-of)
(native jazz:object?)
(native jazz:type?)
(native jazz:category?)
(native jazz:interface?)
(native jazz:method?)
(native jazz:is?)
(native jazz:subtype?)
(native jazz:subcategory?)
(native jazz:subclass?)
(native jazz:iterate-class-overrides)
(native jazz:update-method)


;;;
;;;; Parameters
;;;


(native make-parameter)


;;;
;;;; Pathname
;;;


(native jazz:pathname-type)
(native jazz:pathname-expand)
(native jazz:pathname-normalize)
(native jazz:file-exists?)
(native jazz:file-delete)
(native jazz:file-copy)
(native jazz:file-access-time)
(native jazz:file-modification-time)
(native jazz:file-modification-seconds)
(native jazz:file-times-set!)
(native jazz:file-rename)
(native jazz:add-extension)
(native jazz:current-directory)
(native jazz:current-directory-set!)
(native jazz:with-current-directory)
(native jazz:directory-create)
(native jazz:directory-content)
(native jazz:directory-delete)
(native path-normalize)


;;;
;;;; Pipe
;;;


(native open-string-pipe)
(native open-u8vector-pipe)


;;;
;;;; Port
;;;


(native port?)
(native jazz:close-port)
(native jazz:input-port-timeout-set!)
(native jazz:output-port-timeout-set!)
(native jazz:controlling-terminal?)
(native open-event-queue)
(native jazz:eof-object)
;(native open-input-string)
;(native open-output-string)
;(native get-output-string)
;(native call-with-input-string)
;(native with-input-from-string)
;(native call-with-output-string)
;(native with-output-to-string)
(native read-substring)
(native open-vector)
(native call-with-input-u8vector)
(native call-with-output-u8vector)
(native open-u8vector)
(native open-output-u8vector)
(native get-output-u8vector <any:u8vector>)
(native jazz:read-u8)
(native jazz:write-u8)
(native jazz:read-subu8vector)
(native jazz:write-subu8vector)
(native write-substring)
(native jazz:write-string)

;(native jazz:read-line)
(native jazz:read-proper-line)
(native jazz:read-all)
(native jazz:print)
;(native jazz:pretty-print)
(native jazz:read-source-all)
(native jazz:read-source-first)
(native jazz:read-literal-hook)
(native force-output)
(native current-error-port)
(native with-output-to-port)


;;;
;;;; Procedure
;;;


(native jazz:procedure-name)
(native jazz:procedure-name-set!)
(native jazz:procedure-locat)


;;;
;;;; Property
;;;


(native jazz:new-property)
(native jazz:property-getter)
(native jazz:property-setter)


;;;
;;;; Queue
;;;


(native jazz:new-queue)
(native jazz:queue-empty?)
(native jazz:enqueue)
(native jazz:enqueue-list)
(native jazz:dequeue)
(native jazz:queue-list <any:list>)
(native jazz:trim-queue)
(native jazz:reset-queue)


;;;
;;;; Random
;;;


(native random-source?)
(native make-random-source)
(native random-source-state-ref)
(native random-source-state-set!)
(native random-source-make-integers)
(native random-source-make-reals)
(native jazz:random-integer)
(native jazz:random-real <:fl>)
(native jazz:random-source-randomize!)
(native jazz:random-source-pseudo-randomize!)
(native jazz:default-random-source)


;;;
;;;; Readtable
;;;


(native jazz:readtable?)
(native current-readtable)
(native jazz:make-jazz-readtable)
(native jazz:make-standard-readtable)
(native jazz:readtable-copy)
(native jazz:readtable-char-delimiter?)
(native jazz:readtable-char-delimiter?-set!)
(native jazz:readtable-char-handler)
(native jazz:readtable-char-handler-set!)
(native jazz:readtable-char-sharp-handler)
(native jazz:readtable-char-sharp-handler-set!)
(native jazz:readtable-char-class-set!)
(native jazz:readtable-paren-keyword-set!)
(native jazz:readtable-bracket-keyword-set!)
(native jazz:readtable-brace-keyword-set!)
(native jazz:readtable-named-char-table)
(native jazz:readtable-named-char-table-set!)
(native jazz:readtable-escaped-char-table)
(native jazz:readtable-escaped-char-table-set!)
(native readtable-comment-handler-set)
(native jazz:with-readtable)
(native jazz:scheme-readtable)
(native jazz:jazz-readtable)
(native jazz:with-jazz-readtable)
(native input-port-readtable)
(native input-port-readtable-set!)
(native readtable-start-syntax-set)
(native six-types)
(native six-types-set!)
(native print-marker)


;;;
;;;; Reference
;;;


(native jazz:resolve-runtime-reference)
(native jazz:serialize-runtime-reference)
(native jazz:deserialize-runtime-reference)


;;;
;;;; Repl
;;;


(native jazz:current-repl-context)
(native jazz:repl-context-level)
(native jazz:repl-context-depth)
(native jazz:repl-context-cont)
(native jazz:repl-context-initial-cont)
(native jazz:repl-context-prev-level)
(native jazz:repl-context-prev-depth)
(native jazz:with-repl-context)
(native jazz:inspect-repl-context)
(native jazz:repl)
(native jazz:repl-debug)
(native jazz:eval-within-no-winding)
(native jazz:repl-result-history-add)
(native repl-result-history-ref)


;;;
;;;; Resource
;;;


(native jazz:resource-pathname)


;;;
;;;; Runtime
;;;


(native jazz:get-object-slot)
(native jazz:set-object-slot)
(native jazz:find-slot-offset)
(native jazz:dispatch)
(native jazz:find-dispatch)
(native jazz:call-into-abstract)
(native jazz:get-core-classes)


;;;
;;;; Serial
;;;


(native jazz:object->serial)
(native jazz:serial->object)

;; the -number versions are necessary to support the expansion of the # reader construct
(native object->serial-number)
(native serial-number->object)


;;;
;;;; Shell
;;;


(native shell-command)


;;;
;;;; Slot
;;;


(native jazz:slot? <object:bool>)
(native jazz:slot-value)
(native jazz:set-slot-value)


;;;
;;;; Socket
;;;


(native jazz:socket-info-address)
(native jazz:socket-info-port-number)


;;;
;;;; Stack
;;;


(native jazz:hidden-frame?)
(native jazz:hidden-frame?-set!)
(native jazz:get-continuation-stack)
(native jazz:get-continuation-dynamic-environment)
(native jazz:get-continuation-lexical-environment)
(native jazz:get-continuation-location)
(native jazz:interpreted-continuation?)


;;;
;;;; String
;;;


(native jazz:string-find-reversed)
(native jazz:string-starts-with?)
(native jazz:string-ends-with?)
(native jazz:split-string)
(native jazz:join-strings)


;;;
;;;; Structure
;;;


(native jazz:kind?)
(native jazz:kind-id)
(native jazz:kind-name)
(native jazz:kind-flags)
(native jazz:kind-super)
(native jazz:kind-length)
(native jazz:kind-fields)
(native jazz:structure?)
(native jazz:structure-kind)
(native jazz:structure-ref)
(native jazz:structure-set!)


;;;
;;;; Symbol
;;;


(native gensym)
(native jazz:generate-symbol)
(native jazz:with-uniqueness)
(native jazz:with-uniqueness-typed)


;;;
;;;; System
;;;


(native jazz:switch?)
(native jazz:switch-name)
(native jazz:command-argument)
(native jazz:command-argument?)
(native jazz:open-process)
(native jazz:process-status)
(native jazz:exit)
(native jazz:exit-jobs)
(native jazz:add-exit-job!)
(native jazz:clear-exit-jobs!)
(native make-serialized)
(native serialized?)
(native serialized-class)
(native serialized-content)


;;;
;;;; Table
;;;


;(native table?)
;(native make-table)
;(native table-for-each)
;(native table-ref)
;(native table-set!)
;(native table->list)
;(native list->table)
(native jazz:table-clear)
(native jazz:table-length)
;(native jazz:iterate-table)
(native jazz:iterate-table-safe)
(native jazz:map-table)
(native jazz:table-entries)
(native jazz:eq?-hash)
(native jazz:eqv?-hash)
(native jazz:equal?-hash)
(native jazz:string=?-hash)
(native jazz:string-ci=?-hash)
(native make-will)


;;;
;;;; Terminal
;;;


(native jazz:set-terminal-title)
(native jazz:bring-terminal-to-front)
(native jazz:clear-terminal)


;;;
;;;; Thread
;;;


(native jazz:interrupts-enabled?)
(native jazz:disable-interrupts!)
(native jazz:enable-interrupts!)
(native jazz:thread-heartbeat-interval-set!)
(native jazz:current-thread)
(native jazz:thread?)
(native jazz:make-thread)
(native jazz:make-root-thread)
(native jazz:thread-name)
(native jazz:thread-specific)
(native jazz:thread-specific-set!)
(native jazz:thread-base-priority)
(native jazz:thread-base-priority-set!)
(native jazz:thread-priority-boost)
(native jazz:thread-priority-boost-set!)
(native jazz:thread-quantum)
(native jazz:thread-quantum-set!)
(native jazz:thread-start!)
(native jazz:thread-yield!)
(native jazz:thread-sleep!)
(native jazz:thread-terminate!)
(native jazz:thread-join!)
(native jazz:thread-send)
(native jazz:thread-receive)
(native jazz:thread-mailbox-next)
(native jazz:thread-mailbox-rewind)
(native jazz:thread-mailbox-extract-and-rewind)
(native jazz:thread-interrupt!)
(native jazz:thread-thread-group)
(native jazz:thread-group->thread-group-list)
(native jazz:thread-group->thread-group-vector)
(native jazz:thread-group->thread-list)
(native jazz:thread-group->thread-vector)
(native jazz:thread-state)
(native jazz:thread-state-abnormally-terminated-reason)
(native jazz:thread-state-abnormally-terminated?)
(native jazz:thread-state-active-timeout)
(native jazz:thread-state-active-waiting-for)
(native jazz:thread-state-active?)
(native jazz:thread-state-initialized?)
(native jazz:thread-state-normally-terminated-result)
(native jazz:thread-state-normally-terminated?)
(native jazz:thread-state-uninitialized?)
(native jazz:thread-cont)
(native jazz:thread-continuation)
(native jazz:pristine-thread-continuation)
(native jazz:make-thread-group)
(native jazz:mutex?)
(native jazz:make-mutex)
(native jazz:mutex-name)
(native jazz:mutex-specific)
(native jazz:mutex-specific-set!)
(native jazz:mutex-state)
(native jazz:mutex-lock!)
(native jazz:mutex-unlock!)
(native jazz:mutex-wait)
(native jazz:mutex-owner)
(native jazz:condition?)
(native jazz:make-condition)
(native jazz:condition-name)
(native jazz:condition-specific)
(native jazz:condition-specific-set!)
(native jazz:condition-signal!)
(native jazz:condition-broadcast!)


;;;
;;;; Time
;;;


(native jazz:current-systime)
(native jazz:systime?)
(native jazz:systime->seconds)
(native jazz:seconds->systime)
(native jazz:process-times)
(native jazz:cpu-time <:fl>)
(native jazz:real-time <:fl>)
(native jazz:current-seconds!)
(native jazz:current-seconds <:fl>)


;;;
;;;; Unspecified
;;;


(native jazz:unspecified)
(native jazz:unspecified?)
(native jazz:specified?)


;;;
;;;; Vector
;;;


(native jazz:vector-copy)
(native jazz:subvector-fill!)
(native jazz:subvector-move!)


;;;
;;;; Values
;;;


(native jazz:values?)


;;;
;;;; Walker
;;;


(native jazz:analysis-mode?)
(native jazz:analysis-data)
(native jazz:call-walk-error)
(native jazz:new-walk-context)
(native jazz:specifier?)
(native jazz:binding-specifier)
(native jazz:parse-specifier)
(native jazz:type->specifier)
(native jazz:requested-unit-name)
(native jazz:requested-unit-resource)
(native jazz:get-private-lookup)
(native jazz:get-public-lookup)
(native jazz:get-protected-lookup)
(native jazz:get-catalog)
(native jazz:get-catalog-entry)
(native jazz:release-catalog-entries)
(native jazz:outline-feedback)
(native jazz:get-outline-hook)
(native jazz:set-outline-hook)
(native jazz:outline-unit)
(native jazz:walk-unit)
(native jazz:generate-unit)
(native jazz:expand)
(native jazz:expand-unit)
(native jazz:expand-form)
(native jazz:expand-source)
(native jazz:expand-to-port)
(native jazz:expand-to-file)
(native jazz:expand-script)
(native jazz:find-declaration)
(native jazz:find-declaration-child)
(native jazz:remove-declaration-child))

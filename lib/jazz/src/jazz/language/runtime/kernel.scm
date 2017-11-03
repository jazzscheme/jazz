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
(native jazz:closure-environment)


;;;
;;;; Continuation
;;;


(native jazz:continuation?)
(native jazz:continuation-capture)
(native jazz:continuation-graft)
(native jazz:continuation-graft-no-winding)
(native jazz:continuation-return)
(native jazz:continuation-parent)
(native jazz:continuation-creator)
(native jazz:continuation-locat)
(native jazz:continuation-next)
(native jazz:continuation-frames)
(native jazz:track)


;;;
;;;; Core
;;;


(native jazz:compile-unit)
(native jazz:build-unit)
(native jazz:build-image)
(native jazz:build-library)
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
(native jazz:object->vector)
(native jazz:vector->object)
(native jazz:object-copy)
(native jazz:install-step-handler)


;;;
;;;; Devel
;;;


(native jazz:subtype)


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
;;;; Enumerator
;;;


(native jazz:enumerator?)
(native jazz:enumerator->symbol)
(native jazz:symbol->enumerator)


;;;
;;;; Exception
;;;


(native jazz:exception-reason)
(native jazz:exception-detail)
(native jazz:exception-location)
(native jazz:display-exception)
(native jazz:display-continuation-backtrace)
(native jazz:get-exception-hook)
(native jazz:set-exception-hook)
(native jazz:invoke-exception-hook)
(native jazz:system-exception-hook)
(native jazz:current-exception-handler)
(native jazz:with-exception-handler)
(native jazz:with-exception-catcher)
(native jazz:with-exception-filter)
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


(native jazz:fifo->list)


;;;
;;;; Fixnum
;;;


(native jazz:fixnum->flonum <fx:fl>)
(native jazz:flonum->fixnum <fl:fx>)
(native jazz:+infinity)
(native jazz:-infinity)


;;;
;;;; Flonum
;;;


;; until proper call site casting of native calls

(native undocumented jazz:sharp/sharp/fl+ <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl- <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl* <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl/ <fl^fl:fl>)

(native jazz:flalloc <:fl>)
(native jazz:flset!)


;;;
;;;; Foreign
;;;


(native jazz:foreign?)
(native jazz:foreign-address)
(native jazz:foreign-release!)
(native jazz:foreign-released?)
(native jazz:foreign-tags)
(native jazz:still-obj-refcount-dec!)
(native jazz:still-obj-refcount-inc!)
(native jazz:still-copy)


;;;
;;;; Heartbeat
;;;


(native jazz:jazz-heartbeat)


;;;
;;;; Host
;;;


(native jazz:command-executable)
(native jazz:command-arguments)


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
;;;; Integer
;;;


(native jazz:arithmetic-shift-left <int^int:int>)
(native jazz:arithmetic-shift-right <int^int:int>)


;;;
;;;; Kernel
;;;


(native jazz:build-feedback)
(native jazz:boot-directory)
(native jazz:kernel-compiler)
(native jazz:kernel-system)
(native jazz:kernel-platform)
(native jazz:kernel-processor)
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
(native jazz:kernel-boot)
(native jazz:install-path)
(native jazz:bundle-depth)
(native jazz:get-update-version)
(native jazz:get-update-targets)
(native jazz:get-update-description)
(native jazz:get-jazz-version-number)
(native jazz:jazz-settings-directory)
(native jazz:jazz-settings-version)
(native jazz:list->updates)
(native jazz:home-directory)
(native jazz:versioned-directory)
(native jazz:set-exit-callable)
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
(native jazz:repositories-get)
(native jazz:build-repository-get)
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
(native jazz:package-title)
(native jazz:package-description)
(native jazz:package-authors)
(native jazz:package-stage)
(native jazz:resource-package)
(native jazz:resource-path)
(native jazz:resource-extension)
(native jazz:split-version)
(native jazz:present-version)
(native jazz:load-package)
(native jazz:load-debuggee)
(native jazz:register-run)
(native jazz:registered-run)
(native jazz:run-registered)
(native jazz:register-product)
(native jazz:register-product-run)
(native jazz:get-product-descriptor)
(native jazz:find-product-descriptor)
(native jazz:product-descriptor-named?)
(native jazz:product-descriptor-name)
(native jazz:product-descriptor-title)
(native jazz:product-descriptor-description)
(native jazz:product-descriptor-icon)
(native jazz:product-descriptor-run)
(native jazz:product-descriptor-product)
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
(native jazz:find-unit-product)
(native jazz:find-unit-options)
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
(native jazz:jazz-pathname)
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
(native jazz:get-environment-table)
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
(native jazz:get-modules-table)
(native jazz:find-module)
(native jazz:require-module)
(native jazz:module-get)
(native jazz:module-ref)
(native jazz:module-set!)
(native jazz:with-extension-reader)
(native jazz:with-resource-reader)
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
(native jazz:find-service)
(native jazz:require-service)
(native jazz:register-coupler)
(native jazz:get-couplers)
(native jazz:activate-couplers)
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
(native jazz:compiler-name)
(native jazz:compiler-extension)
(native jazz:language-extension)
(native jazz:add-exit-job!)
(native jazz:log-backtrace)
(native jazz:set-crash-reporter)
(native jazz:crash-process)
(native jazz:invoke-process)
(native jazz:call-process)
(native jazz:feedback)
(native jazz:reporting?)
(native jazz:report)
(native jazz:unit-loadable?)
(native jazz:unit-obj-uptodate?)
(native jazz:load-foundation)
(native jazz:load-runtime)
(native jazz:load-build)
(native jazz:split-command-line)
(native jazz:quit)


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
(native jazz:sort-list)

;; until high-level optimized
(native jazz:kernel-some?)
(native jazz:kernel-every?)
(native jazz:kernel-collect-if)


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
(native jazz:clear-gc-interrupt-jobs!)
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
(native jazz:tcp-client-self-socket-info)
(native jazz:tcp-client-peer-socket-info)
(native jazz:tcp-server-socket-info)
(native jazz:call-with-tcp-client)


;;;
;;;; Object
;;;


(native jazz:jazz?)
(native jazz:jazzify)
(native jazz:jazzstruct?)
(native jazz:jazzstructify)
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
(native jazz:with-current-directory)
(native jazz:directory-create)
(native jazz:directory-content)
(native jazz:directory-delete)


;;;
;;;; Port
;;;


(native jazz:close-port)
(native jazz:input-port-timeout-set!)
(native jazz:output-port-timeout-set!)
(native jazz:controlling-terminal?)
(native jazz:eof-object)
(native jazz:read-u8)
(native jazz:write-u8)
(native jazz:read-subu8vector)
(native jazz:write-subu8vector)
(native jazz:write-string)
(native jazz:read-proper-line)
(native jazz:read-all)
(native jazz:read-source-all)
(native jazz:read-source-first)
(native jazz:read-literal-hook)
(native jazz:character-port-output-width-set!)


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


(native jazz:random-integer)
(native jazz:random-real <:fl>)
(native jazz:random-source-randomize!)
(native jazz:random-source-pseudo-randomize!)
(native jazz:default-random-source)
(native jazz:random-integer-65536)


;;;
;;;; Readtable
;;;


(native jazz:readtable?)
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
(native jazz:with-readtable)
(native jazz:scheme-readtable)
(native jazz:jazz-readtable)
(native jazz:with-jazz-readtable)
(native jazz:six-types)
(native jazz:six-types-set!)
(native jazz:print-marker)


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


;;;
;;;; Resource
;;;


(native jazz:resource-package)
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
(native jazz:exit-no-jobs)
(native jazz:exit-jobs)
(native jazz:add-exit-job!)
(native jazz:clear-exit-jobs!)


;;;
;;;; Table
;;;


(native jazz:table-clear)
(native jazz:table-length)
(native jazz:iterate-table-safe)
(native jazz:map-table)
(native jazz:table-entries)
(native jazz:eq?-hash)
(native jazz:eqv?-hash)
(native jazz:equal?-hash)
(native jazz:string=?-hash)
(native jazz:string-ci=?-hash)


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
(native jazz:get-heartbeat-interval)
(native jazz:set-heartbeat-interval!)
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
(native jazz:thread-mutexes)
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
(native jazz:current-monotonic)


;;;
;;;; Tracking
;;;


(native jazz:tracking-allocations?)
(native jazz:track-allocations)
(native jazz:untrack-allocations)
(native jazz:reset-allocations)
(native jazz:count-allocations)
(native jazz:all-allocations)
(native jazz:snapshot-allocations)
(native jazz:get-allocation-object)
(native jazz:get-allocation-file)
(native jazz:get-allocation-line)
(native jazz:get-allocation-stack)
(native jazz:get-allocation)
(native jazz:with-track-allocations)


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
(native jazz:values-ref)
(native jazz:values-set!)


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
(native jazz:add-primitive-type)
(native jazz:primitive-patterns-get)
(native jazz:primitive-predicates-get)
(native jazz:requested-unit-name)
(native jazz:requested-unit-resource)
(native jazz:get-private-lookup)
(native jazz:get-public-lookup)
(native jazz:get-protected-lookup)
(native jazz:get-catalog-table)
(native jazz:get-catalog-entry)
(native jazz:release-catalog-entries)
(native jazz:outline-feedback)
(native jazz:get-outline-hook)
(native jazz:set-outline-hook)
(native jazz:outline-unit)
(native jazz:walk-unit)
(native jazz:walk/outline-unit)
(native jazz:walk-extended-definition-declaration)
(native jazz:walk-extended-definition)
(native jazz:walk-describe)
(native jazz:generate-unit)
(native jazz:check-unit)
(native jazz:verify-unit)
(native jazz:custom-compile-unit)
(native jazz:expanding-unit)
(native jazz:expand-unit)
(native jazz:expand-form)
(native jazz:expand-source)
(native jazz:expand-to)
(native jazz:expand-to-port)
(native jazz:expand-to-file)
(native jazz:expanding-script)
(native jazz:expand-script)
(native jazz:find-declaration)
(native jazz:find-declaration-child)
(native jazz:remove-declaration-child)
(native jazz:reset-module-imported)
(native jazz:get-unit/module-container)
(native jazz:get-unit/module-requires))

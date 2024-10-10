;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Jazz Dialect Kernel
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


(module protected jazz.dialect.kernel scheme


;;;
;;;; Syntactic Closure
;;;


(native jazz:make-syntactic-closure)
(native jazz:syntactic-closure?)
(native jazz:syntactic-closure-form)
(native jazz:unwrap-syntactic-closure)
(native jazz:strip-syntactic-closures)
(native jazz:strip-source-info)
(native jazz:identifier?)
(native jazz:identifier=?)
(native jazz:sc-macro-transformer)
(native jazz:rsc-macro-transformer)
(native jazz:er-macro-transformer)


;;;
;;;; Syntax
;;;


(native jazz:source?)
(native jazz:text-source?)
(native jazz:source-code)
(native jazz:source-locat)
(native jazz:desourcify)
(native jazz:desourcify-all)
(native jazz:sourcify)
(native jazz:sourcify-if)
(native jazz:sourcify-deep)
(native jazz:sourcify-deep-if)
(native jazz:extract-location)
(native jazz:present-source)
(native jazz:save-emit-to)
(native jazz:locat-container)
(native jazz:locat-position)
(native jazz:locat-start)
(native jazz:locat-end)
(native jazz:locat->container/line/col)
(native jazz:locat->path/container/start/end&)
(native jazz:container->path)
(native jazz:position->filepos)
(native jazz:filepos-line)
(native jazz:filepos-col)


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
;;;; Binary
;;;


(native scan-s8)
(native put-s8)
(native scan-u8)
(native put-u8)
(native scan-s16)
(native put-s16)
(native scan-s16-big-endian)
(native put-s16-big-endian)
(native scan-u16)
(native put-u16)
(native scan-s32)
(native put-s32)
(native scan-u32)
(native put-u32)
(native scan-s32-big-endian)
(native put-s32-big-endian)
(native scan-u32-big-endian)
(native put-u32-big-endian)
(native scan-s64)
(native put-s64)
(native scan-s64-big-endian)
(native put-s64-big-endian)
(native scan-u64)
(native put-u64)
(native scan-float)
(native put-float)
(native scan-floats32!)
(native scan-floats64!)
(native scan-float-big-endian)
(native put-float-big-endian)
(native scan-double)
(native put-double)
(native scan-double-big-endian)
(native put-double-big-endian)
(native scan-c-string)
(native put-c-string)
(native scan-size-string)
(native put-size-string)
(native scan-utf-8-string)
(native put-utf-8-string)


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
;;;; Configure
;;;


(native jazz:enable-track-scheme?)
(native jazz:enable-debug-garbage-collect?)


;;;
;;;; Continuation
;;;


(native jazz:continuation-graft-no-winding)
(native jazz:continuation-needs-winding?)
(native jazz:continuation-parent)
(native jazz:continuation-creator)
(native jazz:continuation-locat)
(native jazz:continuation-next)
(native jazz:continuation-backtrace)


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
(native jazz:terminal-newline)
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


(native ?)  (native get-?)  (native set-?)
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
(native ?0) (native get-?0) (native set-?0)
(native ?1) (native get-?1) (native set-?1)
(native ?2) (native get-?2) (native set-?2)
(native ?3) (native get-?3) (native set-?3)
(native ?4) (native get-?4) (native set-?4)
(native ?5) (native get-?5) (native set-?5)
(native ?6) (native get-?6) (native set-?6)
(native ?7) (native get-?7) (native set-?7)
(native ?8) (native get-?8) (native set-?8)
(native ?9) (native get-?9) (native set-?9)


(native %)  (native get-%)  (native set-%)
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
(native %0) (native get-%0) (native set-%0)
(native %1) (native get-%1) (native set-%1)
(native %2) (native get-%2) (native set-%2)
(native %3) (native get-%3) (native set-%3)
(native %4) (native get-%4) (native set-%4)
(native %5) (native get-%5) (native set-%5)
(native %6) (native get-%6) (native set-%6)
(native %7) (native get-%7) (native set-%7)
(native %8) (native get-%8) (native set-%8)
(native %9) (native get-%9) (native set-%9)


(native $)  (native get-$)  (native set-$)
(native $a) (native get-$a) (native set-$a)
(native $b) (native get-$b) (native set-$b)
(native $c) (native get-$c) (native set-$c)
(native $d) (native get-$d) (native set-$d)
(native $e) (native get-$e) (native set-$e)
(native $f) (native get-$f) (native set-$f)
(native $g) (native get-$g) (native set-$g)
(native $h) (native get-$h) (native set-$h)
(native $i) (native get-$i) (native set-$i)
(native $j) (native get-$j) (native set-$j)
(native $k) (native get-$k) (native set-$k)
(native $l) (native get-$l) (native set-$l)
(native $m) (native get-$m) (native set-$m)
(native $n) (native get-$n) (native set-$n)
(native $o) (native get-$o) (native set-$o)
(native $p) (native get-$p) (native set-$p)
(native $q) (native get-$q) (native set-$q)
(native $r) (native get-$r) (native set-$r)
(native $s) (native get-$s) (native set-$s)
(native $t) (native get-$t) (native set-$t)
(native $u) (native get-$u) (native set-$u)
(native $v) (native get-$v) (native set-$v)
(native $w) (native get-$w) (native set-$w)
(native $x) (native get-$x) (native set-$x)
(native $y) (native get-$y) (native set-$y)
(native $z) (native get-$z) (native set-$z)
(native $0) (native get-$0) (native set-$0)
(native $1) (native get-$1) (native set-$1)
(native $2) (native get-$2) (native set-$2)
(native $3) (native get-$3) (native set-$3)
(native $4) (native get-$4) (native set-$4)
(native $5) (native get-$5) (native set-$5)
(native $6) (native get-$6) (native set-$6)
(native $7) (native get-$7) (native set-$7)
(native $8) (native get-$8) (native set-$8)
(native $9) (native get-$9) (native set-$9)


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
;;;; Exception
;;;


(native jazz:exception-reason)
(native jazz:exception-detail)
(native jazz:exception-locat)
(native jazz:get-exception-hook)
(native jazz:set-exception-hook)
(native jazz:invoke-exception-hook)
(native jazz:system-exception-hook)
(native jazz:handle-exception-filter)
(native jazz:catch-exception-filter)
(native jazz:retry-exception-handler)
(native jazz:dump-exception)
(native jazz:new-walk-problems)


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
(native jazz:->flonum <:fl>)


;;;
;;;; Flonum
;;;


;; until proper call site casting of native calls

(native undocumented jazz:sharp/sharp/fl+ <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl- <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl* <fl^fl:fl>)
(native undocumented jazz:sharp/sharp/fl/ <fl^fl:fl>)

(native jazz:flalloc <:fl>)
(native jazz:flref)
(native jazz:flset!)


;;;
;;;; Foreign
;;;


(native jazz:still-obj-refcount-dec!)
(native jazz:still-obj-refcount-inc!)
(native jazz:still-copy)


;;;
;;;; Foundation
;;;


(native jazz:debug-core?)
(native jazz:debug-user?)
(native jazz:valid-conditional-requirement)
(native jazz:conditional-satisfied?)
(native jazz:process-conditional)
(native jazz:cond-expand-features)
(native jazz:cond-expand-features-set!)
(native jazz:cond-expand-features-add!)
(native jazz:feature-satisfied?)
(native jazz:generate-symbol)
(native jazz:simplify-begin)
(native jazz:compose-reference)


;;;
;;;; Heartbeat
;;;


(native jazz:thread-heartbeat!)
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
;;;; Install
;;;


(cond-expand
  (windows
   (native CSIDL_PERSONAL)
   (native CSIDL_DESKTOPDIRECTORY)
   (native CSIDL_APPDATA)
   (native CSIDL_LOCAL_APPDATA)
   (native CSIDL_PROGRAM_FILES)
   (native get-special-folder))
  (else))


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
(native jazz:kernel-features)
(native jazz:kernel-properties)
(native jazz:kernel-built)
(native jazz:kernel-path)
(native jazz:kernel-install)
(native jazz:kernel-bundle-contents)
(native jazz:kernel-bundle-root)
(native jazz:kernel-bundle-install)
(native jazz:kernel-root)
(native jazz:kernel-source)
(native jazz:kernel-source-root)
(native jazz:kernel-source-accessible?)
(native jazz:kernel-source-access?)
(native jazz:kernel-version)
(native jazz:kernel-boot)
(native jazz:kernel-boot-monotonic)
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
(native jazz:path->container-override)
(native jazz:platform-eol-encoding)
(native jazz:load-debuggee-units)
(native jazz:jazz-product)
(native jazz:jazz-profile)
(native jazz:unit-verbose?)
(native jazz:load-verbose?)
(native jazz:outline-verbose?)
(native jazz:shape-verbose?)
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
(native jazz:package-units-root)
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
(native jazz:set-link-options)
(native jazz:current-process-product)
(native jazz:current-process-name)
(native jazz:current-process-name-set!)
(native jazz:current-process-title)
(native jazz:current-process-title-set!)
(native jazz:current-process-prefix)
(native jazz:current-process-prefix-set!)
(native jazz:current-process-traits)
(native jazz:current-process-traits-set!)
(native jazz:current-process-icon)
(native jazz:current-process-icon-set!)
(native jazz:current-process-version)
(native jazz:current-process-version-set!)
(native jazz:current-process-present)
(native jazz:parent-directory)
(native jazz:destination-directory)
(native jazz:executable-extension)
(native jazz:quote-pathname)
(native jazz:quote-jazz-pathname)
(native jazz:jazz-pathname)
(native jazz:set-product-download)
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
(native jazz:verbose-port)
(native jazz:set-verbose-port)
(native jazz:load-unit)
(native jazz:load-count)
(native jazz:load-hook)
(native jazz:load-feedback-setup-count)
(native jazz:load-feedback-done-count)
(native jazz:load-feedback-setup)
(native jazz:load-feedback-done)
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
(native jazz:get-unit-not-found-hook)
(native jazz:set-unit-not-found-hook)
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
(native jazz:global-setting)
(native jazz:testing?)
(native jazz:testing)
(native jazz:generate-symbol-for)
(native jazz:generate-symbol-context)
(native jazz:image-load-counter)
(native jazz:object-load-counter)
(native jazz:interpreted-load-counter)
(cond-expand
  (mac
   (native jazz:use-dlclose))
  (else))
(native jazz:compiler-present?)
(native jazz:compiler-name)
(native jazz:compiler-extension)
(native jazz:language-extension)
(native jazz:codesign-required?)
(native jazz:codesign-if)
(native jazz:codesign)
(native jazz:add-exit-job!)
(native jazz:enable-crash-handler)
(native jazz:disable-crash-handler)
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
(native jazz:unit-source)
(native jazz:split-command-line)
(native jazz:quit)
(native jazz:pkg-config)
(native jazz:pkg-config-exists?)
(native jazz:pkg-config-version)
(native jazz:pkg-config-cflags)
(native jazz:pkg-config-libs)
(native jazz:file-executable?)
(native jazz:file-permissions)
(native jazz:file-permissions-set!)


;;;
;;;; List
;;;


(native jazz:not-null?)
(native jazz:listify)
(native jazz:list-copy)
(native jazz:last-pair)
(native jazz:proper-list)
(native jazz:sort-list)
(native jazz:sort-stable)

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
(native jazz:still8-minimum)
(native jazz:still16-minimum)
(native jazz:still32-minimum)
(native jazz:still64-minimum)
(native MOVABLE0)
(native MOVABLE1)
(native MOVABLE2)
(native STILL)
(native PERM)
(native jazz:memory-allocated?)
(native jazz:memory-allocated-kind)
(native jazz:memory-allocated-size)
(native jazz:memory-size)
(native jazz:gc-hash-table?)


;;;
;;;; Mutation
;;;


(native jazz:loading-module)
(native jazz:get-mutations)
(native jazz:reset-mutations)
(native jazz:register-mutation)


;;;
;;;; Number
;;;


(native jazz:ratnum?)
(native jazz:bignum?)


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
;;;; Pathname
;;;


(native jazz:pathname-type)
(native jazz:pathname-link?)
(native jazz:pathname-expand)
(native jazz:pathname-normalize)
(native jazz:file-last-access-seconds)
(native jazz:file-last-modification-seconds)
(native jazz:add-extension)
(native jazz:copy-file)
(native jazz:current-directory)
(native jazz:with-current-directory)
(native jazz:directory-content)
(native jazz:directory-collect)


;;;
;;;; Port
;;;


(native jazz:stdin-port)
(native jazz:stdout-port)
(native jazz:stderr-port)
(native jazz:standard-input-port)
(native jazz:standard-output-port)
(native jazz:standard-error-port)
(native jazz:unix-controlling-terminal?)
(native jazz:terminal-available?)
(native jazz:filesystem-authorize-terminal)
(native jazz:eof-object)
(native jazz:write-string)
(native jazz:read-proper-line)
(native jazz:read-source-all)
(native jazz:read-source-first)
(native jazz:read-literal-hook)
(native jazz:output-port-width-set!)
(native jazz:debug-port-setup-width)


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
(native jazz:queue-length)
(native jazz:enqueue)
(native jazz:enqueue-list)
(native jazz:dequeue)
(native jazz:queue-list <any:list>)
(native jazz:trim-queue)
(native jazz:reset-queue)


;;;
;;;; Random
;;;


(native jazz:random-integer-65536)


;;;
;;;; Readtable
;;;


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
(native jazz:name->path)
(native jazz:path->name)


;;;
;;;; Restricted
;;;


(native jazz:filesystem-restrictable)
(native jazz:filesystem-restricted?)
(native jazz:filesystem-restrict)
(native jazz:filesystem-unrestrict)
(native jazz:filesystem-authorized)
(native jazz:filesystem-authorize)
(native jazz:filesystem-allowed?)
(native jazz:validate-restricted)


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
(native jazz:find-option)
(native jazz:exit-no-jobs)
(native jazz:exit-jobs)
(native jazz:add-exit-job!)
(native jazz:clear-exit-jobs!)


;;;
;;;; Table
;;;


(native jazz:table-iterate)
(native jazz:table-iterate-safe)
(native jazz:table-clear)
(native jazz:map-table)
(native jazz:table-entries)


;;;
;;;; Terminal
;;;


(native jazz:set-terminal-title)
(native jazz:bring-terminal-to-front)
(native jazz:clear-terminal)


;;;
;;;; Thread
;;;


(native TERMINATE-INTERRUPT)
(native HEARTBEAT-INTERRUPT)
(native USER-INTERRUPT)
(native GC-INTERRUPT)
(native HIGH-LEVEL-INTERRUPT)
(native jazz:interrupt-vector-set!)
(native jazz:interrupts-enabled?)
(native jazz:disable-interrupts!)
(native jazz:enable-interrupts!)
(native jazz:get-heartbeat-interval)
(native jazz:set-heartbeat-interval!)
(native jazz:thread-int!)
(native jazz:thread-cont)
(native jazz:thread-continuation)
(native jazz:pristine-thread-continuation)
(native jazz:thread-mutexes)
(native jazz:write-timeout)


;;;
;;;; Thread With Stack
;;;


(native jazz:thread-add-stack)
(native jazz:make-thread-with-stack)


;;;
;;;; Time
;;;


(native jazz:current-systime)
(native jazz:systime?)
(native jazz:systime->seconds)
(native jazz:seconds->systime)
(native jazz:current-seconds!)
(native jazz:current-seconds <:fl>)
(native jazz:current-monotonic <:fl>)
(native jazz:current-monotonic-jiffies)
(native jazz:current-monotonic-frequency)
(native jazz:current-monotonic-nanoseconds)
(native jazz:present-seconds)
(native jazz:present-bytes)


;;;
;;;; Tracking
;;;


(native jazz:tracking-allocations?)
(native jazz:monitoring-allocations?)
(native jazz:monitor-allocations-reset)
(native jazz:monitor-allocations-start)
(native jazz:monitor-allocations-stop)
(native jazz:monitor-save-rate)
(native jazz:monitor-capture-rate)
(native jazz:monitor-allocation-rate)
(native jazz:register-allocations)
(native jazz:unregister-allocations)
(native jazz:reset-allocations)
(native jazz:registered-allocations)
(native jazz:ordered-allocations)
(native jazz:persist-allocations)
(native jazz:unpersist-allocations)
(native jazz:persisted-allocations-table)
(native jazz:allocation-rank)
(native jazz:allocation-size)
(native jazz:allocation-thread)
(native jazz:allocation-file)
(native jazz:allocation-line)
(native jazz:allocation-stack)


;;;
;;;; UDP
;;;


(native jazz:udp-socket-tos)
(native jazz:udp-socket-tos-set!)
(native jazz:udp-socket-receive-buffer-size)
(native jazz:udp-socket-receive-buffer-size-set!)
(native jazz:udp-socket-send-buffer-size)
(native jazz:udp-socket-send-buffer-size-set!)
(native jazz:udp-socket-send-again-count)


;;;
;;;; Unspecified
;;;


(native jazz:unspecified)
(native jazz:unspecified?)
(native jazz:specified?)


;;;
;;;; Values
;;;


(native jazz:values?)
(native jazz:values-ref)
(native jazz:values-set!)


;;;
;;;; Vector
;;;


(native jazz:vector-memq?)
(native jazz:vector-memv?)


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
(native jazz:new-fixed-type)
(native jazz:fixed-constructors-get)
(native jazz:fixed-makers-get)
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
(native jazz:get-outline-not-found-hook)
(native jazz:set-outline-not-found-hook)
(native jazz:get-outline-hook)
(native jazz:set-outline-hook)
(native jazz:outline-unit)
(native jazz:walk-unit)
(native jazz:walk/outline-unit)
(native jazz:walk-extended-definition-declaration)
(native jazz:walk-extended-definition)
(native jazz:walk-describe)
(native jazz:generate-unit)
(native jazz:emit-unit)
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

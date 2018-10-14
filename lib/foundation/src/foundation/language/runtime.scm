;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Foundation Language Runtime
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


(module foundation.language.runtime foundation.dialect


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
;;;; Kernel
;;;


(native jazz:debug-core?)
(native jazz:debug-user?)
(native jazz:valid-conditional-requirement)
(native jazz:conditional-satisfied?)
(native jazz:process-conditional)
(native jazz:cond-expand-features)
(native jazz:feature-satisfied?)
(native jazz:generate-symbol)
(native jazz:simplify-begin)
(native jazz:compose-reference)


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
;;;; Table
;;;


(native jazz:iterate-table))

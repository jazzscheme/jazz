;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Install
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


(jazz.kernel-declare)


(cond-expand
  (mac
    (define jazz.executable-directory
      #f))
  (windows
    (define (jazz.executable-directory)
      (jazz.pathname-dir (jazz.pathname-normalize (GetModuleFileName)))))
  (else
    (define jazz.executable-directory
      #f)))


;;;
;;;; Platform
;;;


(cond-expand
  #; ;; to complete
  (mac
    (define ExecutablePath
      (c-lambda () char-string
        "ProcessInfoRec info;
         ProcessSerialNumber serial;
         FSSpec spec;
         FSRef ref;
         char path[2048];
         serial.highLongOfPSN = 0;
         serial.lowLongOfPSN = kCurrentProcess;
         info.processInfoLength = sizeof(info);
         info.processName = NULL;
         info.processAppSpec = &spec;
         GetProcessInformation(&serial, &info);
         FSpMakeFSRef(&spec, &ref);
         FSRefMakePath(&ref, (UInt8*)path, 2048);
         ___result = path;")))
  (windows
    (define GetModuleFileName
      (c-lambda () wchar_t-string
        "wchar_t buf[MAX_PATH];
         GetModuleFileNameW(NULL, buf, 300);
         ___result = buf;")))
  (else))

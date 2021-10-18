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


(block kernel.install


(cond-expand
  (mac
    (c-include "<mach-o/dyld.h>"))
  (linux
    (c-include "<unistd.h>")
    (c-include "<stdio.h>"))
  (else))


(cond-expand
  (windows
   (c-include "<shlobj.h>")
   
   (define CSIDL_PERSONAL          5)
   (define CSIDL_DESKTOPDIRECTORY 16)
   (define CSIDL_LOCAL_APPDATA    28)
   (define CSIDL_PROGRAM_FILES    38)
   
   (c-external (get-special-folder int) wchar_t-string
     "wchar_t szDir[MAX_PATH];
      SHGetSpecialFolderPathW(0, szDir, ___arg1, FALSE);
      ___return(szDir);"))
  (else))


(cond-expand
  (mac
    (c-external (jazz:platform-executable-path) char-string
            "char path[1024];
             uint32_t pathLength = 1023;
             _NSGetExecutablePath(path,&pathLength);
             path[1023] = 0;
             ___return(path);"))
  (windows
    (c-external (jazz:platform-executable-path) wchar_t-string
            "wchar_t buf[MAX_PATH];
             GetModuleFileNameW(NULL, buf, 300);
              ___return(buf);"))
  (linux
    (c-external (jazz:platform-executable-path) char-string
            "char link[64];
             char path[1024];
             pid_t pid;
             int result;
             pid = getpid();
             result = snprintf(link, sizeof(link), \"/proc/%i/exe\", pid);
             if (result >= 0) {
               result = readlink(link, path, sizeof(path));
               if( result < 0 || result >= (int)sizeof(path) )
                 ___return(0);
               else {
                 path[result] = 0;
                 ___return(path);
               }
             }
             else
               ___return(0);"))
  (else))


(jazz:define-variable-override jazz:executable-path
  (lambda ()
    (jazz:platform-executable-path)))


(jazz:define-variable-override jazz:executable-directory
  (lambda ()
    (let ((path (jazz:platform-executable-path)))
      (if path
          (jazz:pathname-dir (jazz:pathname-normalize path))
        #f)))))

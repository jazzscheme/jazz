;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel-Level Crash Handler
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
;;;    Louis-Julien Guillemette
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


(block kernel.crash


(define (jazz:log-backtrace ignore)
  (continuation-capture
    (lambda (k)
      (let ((port (current-error-port)))
        (display "Frames:" port)
        (newline port)
        ;; dump frames
        (display-continuation-backtrace k port #f #f 500 500)
        (newline port)
        (display "Variables:" port)
        (newline port)
        ;; dump variables
        (display-continuation-backtrace k port #f #t 500 500)))))


(jazz:set-crash-reporter jazz:log-backtrace)


(cond-expand
  (windows
    
    (define SEM_FAILCRITICALERRORS #x0001)
    
    (define SEM_NOGPFAULTERRORBOX #x0002)
    
    (c-external (SetErrorMode unsigned-int) unsigned-int)
    
    (define (jazz:disable-crash-window)
      (SetErrorMode (bitwise-ior SEM_FAILCRITICALERRORS SEM_NOGPFAULTERRORBOX)))
    
    (c-definition (jazz:call_crash_reporter ignore) ((pointer void)) void "jazz_call_crash_reporter" ""
      (jazz:crash-reporter ignore))

    (c-declaration crash #<<end-of-c-code
      static LONG WINAPI unhandled_exception_filter(LPEXCEPTION_POINTERS info)
      {
        jazz_call_crash_reporter(info);
        return EXCEPTION_CONTINUE_SEARCH;
      }

      static void setup_low_level_windows_crash_handler()
      {
        SetUnhandledExceptionFilter(unhandled_exception_filter);
      }
end-of-c-code
    )
    (c-initialization crash "setup_low_level_windows_crash_handler();")
    
    (c-declaration crash "const DWORD CRASH_PROCESS = (DWORD) 0xE0000001L;")
    
    (c-external (jazz:crash-process) void
      "RaiseException(CRASH_PROCESS, EXCEPTION_NONCONTINUABLE , 0, NULL);"))
  (else
   
   (define (jazz:disable-crash-window)
     #!void)
   
   (c-definition (jazz:call_crash_reporter ignore) (int) void "jazz_call_crash_reporter" ""
     (jazz:crash-reporter ignore))

   (c-definition (jazz:call_crash_exit) () void "jazz_call_crash_exit" ""
     (exit 1))

   (c-declaration crash #<<end-of-c-code
      #include <stdio.h>
      #include <unistd.h>
      #include <sys/types.h>
      #include <signal.h>

      static void error_signal_handler(int sig_num)
      {
        jazz_call_crash_reporter(sig_num);
        fflush(stdout);
        jazz_call_crash_exit();
      }

      static void setup_low_level_unix_crash_handler()
      {
        // core dumping signals
        signal(SIGQUIT, error_signal_handler);
        signal(SIGILL,  error_signal_handler);
        signal(SIGABRT, error_signal_handler);
        signal(SIGFPE,  error_signal_handler);
        signal(SIGBUS,  error_signal_handler);
        signal(SIGSEGV, error_signal_handler);
        signal(SIGSYS,  error_signal_handler);
      }
end-of-c-code
   )

   (c-initialization crash "setup_low_level_unix_crash_handler();")

   (c-external (jazz:crash-process) void
     "raise(SIGSEGV);"))))

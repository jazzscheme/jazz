;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel-Level Logging
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


(block kernel.logging


(c-declare #<<END-OF-DECLARE
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>

const char* logging_filename = "log.txt";

static bool is_logging = true;

static FILE *logging_stream = NULL;

static int logging_line(const char *line)
{
  if (!is_logging)
    return 0;
  
  va_list ap;
  int result;
  FILE *stream;
  
  if (!logging_stream)
    logging_stream = fopen(logging_filename, "w");

  stream = logging_stream;
  
  fprintf (stream, "%s\n", line);

  fflush (stream);

  return result;
}

static int logging(const char *format, ...)
{
  if (!is_logging)
    return 0;
  
  va_list ap;
  int result;
  FILE *stream;
  
  if (!logging_stream)
    logging_stream = fopen(logging_filename, "w");

  stream = logging_stream;
  
  va_start (ap, format);
  result = vfprintf (stream, format, ap);
  va_end (ap);

  fflush (stream);

  return result;
}
END-OF-DECLARE
)


(define enter-marker
  ";---> ")

(define exit-marker
  ";<--- ")


(define scheme-marker
  "")

(define c-marker
  " (C)")


(define jazz:logging-depth
  (make-parameter 0))


(define jazz:logging?
  (c-lambda () bool
    "___return(is_logging);"))

(define jazz:set-logging?
  (c-lambda (bool) void
    "is_logging = ___arg1;"))


(define jazz:logging-line
  (c-lambda (char-string) int
    "logging_line"))


(define (jazz:logging-scheme->c name thunk . args)
  (define (marshall obj)
    (if (foreign? obj)
        (object->serial-number obj)
      (list 'quote obj)))
  
  (if (%%not (jazz:logging?))
      (thunk)
    (let ((str (%%symbol->string name))
          (prefix (make-string (%%fx* (jazz:logging-depth) 2) #\space)))
      (jazz:logging-line (with-output-to-string ""
                           (lambda ()
                             (display "(")
                             (write (cons name (map marshall args))))))
      (let ((result (parameterize ((jazz:logging-depth (%%fx+ (jazz:logging-depth) 1)))
                      (thunk))))
        (jazz:logging-line (with-output-to-string ""
                             (lambda ()
                               (display " ")
                               (write (marshall result))
                               (display ")"))))
        result))))


(define (jazz:logging-c->scheme name thunk)
  (if (%%not (jazz:logging?))
      (thunk)
    (let ((str (%%symbol->string name))
          (prefix (make-string (%%fx* (jazz:logging-depth) 2) #\space)))
      (jazz:logging-line (%%string-append prefix enter-marker str scheme-marker))
      (let ((result (parameterize ((jazz:logging-depth (%%fx+ (jazz:logging-depth) 1)))
                      (thunk))))
        (jazz:logging-line (%%string-append prefix exit-marker str c-marker))
        result)))))

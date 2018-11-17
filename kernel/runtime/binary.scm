;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; Kernel Binary
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


(block kernel.binary


(c-include "<string.h>")


(c-external (scan-s8 scheme-object int) int8
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___S8*) (ptr + index));
end-of-code
)


(c-external (put-s8 scheme-object int int8) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___S8*) (ptr + index) = ___arg3;
end-of-code
)


(c-external (scan-u8 scheme-object int) unsigned-int8
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___U8*) (ptr + index));
end-of-code
)


(c-external (put-u8 scheme-object int unsigned-int8) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___U8*) (ptr + index) = ___arg3;
end-of-code
)


(c-external (scan-s16 scheme-object int) int16
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___S16*) (ptr + index));
end-of-code
)


(c-external (put-s16 scheme-object int int16) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___S16*) (ptr + index) = ___arg3;
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (scan-s16-big-endian scheme-object int) int16
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[2];
    data[1] = *scan++;
    data[0] = *scan++;
    ___return(* (___S16*) data);
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (put-s16-big-endian scheme-object int int16) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[2];
    * (___S16*) data = ___arg3;
    *scan++ = data[1];
    *scan++ = data[0];
end-of-code
)


(c-external (scan-u16 scheme-object int) unsigned-int16
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___U16*) (ptr + index));
end-of-code
)


(c-external (put-u16 scheme-object int unsigned-int16) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___U16*) (ptr + index) = ___arg3;
end-of-code
)


(c-external (scan-s32 scheme-object int) int32
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___S32*) (ptr + index));
end-of-code
)


(c-external (put-s32 scheme-object int int32) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___S32*) (ptr + index) = ___arg3;
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (scan-s32-big-endian scheme-object int) int32
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[4];
    data[3] = *scan++;
    data[2] = *scan++;
    data[1] = *scan++;
    data[0] = *scan++;
    ___return(* (___S32*) data);
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (put-s32-big-endian scheme-object int int32) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[4];
    * (___S32*) data = ___arg3;
    *scan++ = data[3];
    *scan++ = data[2];
    *scan++ = data[1];
    *scan++ = data[0];
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (scan-u32-big-endian scheme-object int) unsigned-int32
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[4];
    data[3] = *scan++;
    data[2] = *scan++;
    data[1] = *scan++;
    data[0] = *scan++;
    ___return(* (___U32*) data);
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (put-u32-big-endian scheme-object int unsigned-int32) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[4];
    * (___U32*) data = ___arg3;
    *scan++ = data[3];
    *scan++ = data[2];
    *scan++ = data[1];
    *scan++ = data[0];
end-of-code
)


(c-external (scan-s64 scheme-object int) int64
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___S64*) (ptr + index));
end-of-code
)


(c-external (put-s64 scheme-object int int64) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___S64*) (ptr + index) = ___arg3;
end-of-code
)


(c-external (scan-u64 scheme-object int) unsigned-int64
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (___U64*) (ptr + index));
end-of-code
)


(c-external (put-u64 scheme-object int unsigned-int64) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (___U64*) (ptr + index) = ___arg3;
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (scan-s64-big-endian scheme-object int) int64
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[8];
    data[7] = *scan++;
    data[6] = *scan++;
    data[5] = *scan++;
    data[4] = *scan++;
    data[3] = *scan++;
    data[2] = *scan++;
    data[1] = *scan++;
    data[0] = *scan++;
    ___return(* (___S64*) data);
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (put-s64-big-endian scheme-object int int64) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[8];
    * (___S64*) data = ___arg3;
    *scan++ = data[7];
    *scan++ = data[6];
    *scan++ = data[5];
    *scan++ = data[4];
    *scan++ = data[3];
    *scan++ = data[2];
    *scan++ = data[1];
    *scan++ = data[0];
end-of-code
)


(c-external (scan-float scheme-object int) float
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (float*) (ptr + index));
end-of-code
)


(c-external (put-float scheme-object int float) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (float*) (ptr + index) = ___arg3;
end-of-code
)


(c-external (scan-floats32! scheme-object int scheme-object int) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    float *scan = (float*) (ptr + index);
    float *dest = ___CAST(float*,___BODY(___arg3));
    int count = ___arg4;
    int i;
    for (i = 0; i < count; i++)
        *dest++ = *scan++;
end-of-code
)


(c-external (scan-floats64! scheme-object int scheme-object int) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    float *scan = (float*) (ptr + index);
    double *dest = ___CAST(double*,___BODY(___arg3));
    int count = ___arg4;
    int i;
    for (i = 0; i < count; i++)
        *dest++ = *scan++;
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (scan-float-big-endian scheme-object int) float
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[4];
    data[3] = *scan++;
    data[2] = *scan++;
    data[1] = *scan++;
    data[0] = *scan++;
    ___return(* (float*) data);
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (put-float-big-endian scheme-object int float) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[4];
    * (float*) data = ___arg3;
    *scan++ = data[3];
    *scan++ = data[2];
    *scan++ = data[1];
    *scan++ = data[0];
end-of-code
)


(c-external (scan-double scheme-object int) double
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(* (double*) (ptr + index));
end-of-code
)


(c-external (put-double scheme-object int double) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    * (double*) (ptr + index) = ___arg3;
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (scan-double-big-endian scheme-object int) double
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[8];
    data[7] = *scan++;
    data[6] = *scan++;
    data[5] = *scan++;
    data[4] = *scan++;
    data[3] = *scan++;
    data[2] = *scan++;
    data[1] = *scan++;
    data[0] = *scan++;
    ___return(* (double*) data);
end-of-code
)


;; quick hack hardcoded for little endian machines
(c-external (put-double-big-endian scheme-object int double) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *scan = ptr + index;
    char data[8];
    * (double*) data = ___arg3;
    *scan++ = data[7];
    *scan++ = data[6];
    *scan++ = data[5];
    *scan++ = data[4];
    *scan++ = data[3];
    *scan++ = data[2];
    *scan++ = data[1];
    *scan++ = data[0];
end-of-code
)


(c-external (scan-c-string scheme-object int int) char-string
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    int size = ___arg3;
    char str[size];
    memcpy(str, ptr + index, size);
    ___return(str);
end-of-code
)


(c-external (put-c-string scheme-object int char-string int int) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *str = ___arg3;
    int strlen = ___arg4;
    int size = ___arg5;
    memcpy(ptr + index, str, strlen + 1);
end-of-code
)


(c-external (scan-size-string scheme-object int int) char-string
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    int size = ___arg3;
    char str[size + 1];
    memcpy(str, ptr + index, size);
    str[size] = 0;
    ___return(str);
end-of-code
)


(c-external (put-size-string scheme-object int char-string int) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    char *str = ___arg3;
    int size = ___arg4;
    memcpy(ptr + index, str, size);
end-of-code
)


(c-external (scan-utf-8-string scheme-object int) UTF-8-string
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    ___return(ptr + index);
end-of-code
)


(c-external (put-utf-8-string scheme-object int UTF-8-string int) void
  #<<end-of-code
    char *ptr = ___CAST(char*,___BODY(___arg1));
    int index = ___arg2;
    memcpy(ptr + index, ___arg3, ___arg4);
end-of-code
))

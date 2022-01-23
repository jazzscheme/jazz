;;;==============
;;;  JazzScheme
;;;==============
;;;
;;;; OpenGL Product
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


(unit jazz.opengl.product


;;;
;;;; Build
;;;


(cond-expand
  (windows
    (define jazz:opengl-files
      (list (cons "lib/jazz.opengl/foreign/windows/opengl/glew/bin/glew32.dll" "glew32.dll"))))
  (cocoa
    (define jazz:opengl-files
      (list (cons "lib/jazz.opengl/foreign/mac/opengl/glew/lib/libGLEW.dylib" "Libraries/libGLEW.dylib"))))
  (else
    (define jazz:opengl-files
      (list (cons "lib/jazz.opengl/foreign/linux/opengl/glew/lib/libGLEW.so.1.13" "libGLEW.so.1.13")))))


(define (jazz:copy-opengl-files)
  (let ((source jazz:kernel-source)
        (build (%%get-repository-directory jazz:Build-Repository)))
    (define (source-file path)
      (string-append source path))

    (define (build-file path)
      (string-append build path))

    (for-each (lambda (info)
                (let ((source (car info))
                      (build (cdr info)))
                  (jazz:copy-file-if-needed (source-file source) (build-file build) feedback: jazz:feedback)))
              jazz:opengl-files)))


(cond-expand
  (windows
    (define jazz:opengl-units
      (let ((glew-include-path (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/windows/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/windows/opengl/glew/lib")))
        `((jazz.opengl.glew.foreign cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglew32"))
          (jazz.opengl.glew.header cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglew32"))
          (jazz.opengl.glew.windows cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglew32"))
          (jazz.opengl.platform.windows cc-options: "-DUNICODE" ld-options: "-mwindows -lopengl32")))))
  (cocoa
    (define jazz:opengl-units
      (let ((glew-include-path (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/mac/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/mac/opengl/glew/lib")))
        `((jazz.opengl.glew.foreign cc-options: ,(jazz:patch-mac-ld-warnings (string-append "-I" glew-include-path)) ld-options: ,(string-append "-L" glew-lib-path " -framework OpenGL -lGLEW"))
          (jazz.opengl.glew.header cc-options: ,(jazz:patch-mac-ld-warnings (string-append "-I" glew-include-path)) ld-options: ,(string-append "-L" glew-lib-path " -framework OpenGL -lGLEW"))))))
  (x11
    (define jazz:opengl-units
      (let ((glew-include-path (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/linux/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/linux/opengl/glew/lib")))
        (let ((cc-options (string-append "-I" glew-include-path))
              (ld-options (string-append "-Wl,-rpath,$ORIGIN/../../../../../.." " -L" glew-lib-path " -lGLEW -lGL")))
          `((jazz.opengl.glew.foreign cc-options: ,cc-options ld-options: ,ld-options)
            (jazz.opengl.glew.x11     cc-options: ,cc-options ld-options: ,ld-options)
            (jazz.opengl.glew.header  cc-options: ,cc-options ld-options: ,ld-options))))))
  (else
    (define jazz:opengl-units
      '())))


(define (jazz:build-opengl descriptor #!key (unit #f) (skip-references? #f) (force? #f))
  (let ((unit-specs jazz:opengl-units))
    (jazz:custom-compile/build unit-specs pre-build: jazz:copy-opengl-files unit: unit force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor unit: unit force?: force?))))


(define (jazz:opengl-library-options descriptor add-language)
  (cond-expand
    (windows
      (let ((glew-lib-path (jazz:jazz-pathname "lib/jazz.opengl/foreign/windows/opengl/glew/lib")))
        (list (string-append "-L" glew-lib-path) "-lopengl32" "-lglew32" "-mwindows")))
    (cocoa
      (let ((glew-lib-path (jazz:jazz-pathname "lib/jazz.opengl/foreign/mac/opengl/glew/lib")))
        (list (string-append "-L" glew-lib-path) "-framework" "OpenGL" "-lGLEW")))
    (x11
      (let ((glew-lib-path (jazz:quote-jazz-pathname "lib/jazz.opengl/foreign/linux/opengl/glew/lib")))
        (list (string-append "-L" glew-lib-path) "-lGL")))
    (else
     '())))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.opengl
  build: jazz:build-opengl
  library-options: jazz:opengl-library-options))

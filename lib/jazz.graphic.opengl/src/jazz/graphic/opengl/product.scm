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
;;;  Portions created by the Initial Developer are Copyright (C) 1996-2012
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


(unit jazz.graphic.opengl.product


;;;
;;;; Build
;;;


(cond-expand
  (windows
    (define (jazz:copy-opengl-files)
      (let ((build (%%get-repository-directory jazz:Build-Repository))
            (source jazz:kernel-source))
        (define (build-file path)
          (string-append build path))
        
        (define (source-file path)
          (string-append source path))
        
        (jazz:copy-file (source-file "foreign/opengl/freeglut/bin/freeglut.dll") (build-file "freeglut.dll") feedback: jazz:feedback)
        (jazz:copy-file (source-file "foreign/opengl/glew/bin/glew32.dll") (build-file "glew32.dll") feedback: jazz:feedback))))
  (else
    (define (jazz:copy-opengl-files)
      #f)))


(cond-expand
  (windows
    (define jazz:opengl-units
      (let ((glut-include-path (jazz:quote-jazz-pathname "foreign/opengl/freeglut/include"))
            (glut-lib-path     (jazz:quote-jazz-pathname "foreign/opengl/freeglut/lib"))
            (glew-include-path (jazz:quote-jazz-pathname "foreign/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "foreign/opengl/glew/lib/windows")))
        `((jazz.graphic.opengl.foreign.gl-header)
          (jazz.graphic.opengl.foreign.gl ld-options: "-lopengl32")
          (jazz.graphic.opengl.foreign.glext-header)
          (jazz.graphic.opengl.foreign.glext)
          (jazz.graphic.opengl.foreign.glu-header)
          (jazz.graphic.opengl.foreign.glu ld-options: "-lopengl32 -lglu32")
          (jazz.graphic.opengl.foreign.glut-header)
          (jazz.graphic.opengl.foreign.glut cc-options: ,(string-append "-I" glut-include-path) ld-options: ,(string-append "-L" glut-lib-path " -lopengl32 -lglu32 -lfreeglut"))
          (jazz.graphic.opengl.glew.foreign cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglu32 -lglew32"))
          (jazz.graphic.opengl.glew.header cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglew32"))
          (jazz.graphic.opengl.glew.windows cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -lopengl32 -lglew32"))
          (jazz.graphic.opengl.platform.windows cc-options: "-DUNICODE -D_WIN32_WINNT=0x0502" ld-options: "-mwindows -lopengl32")))))
  (cocoa
    (define jazz:opengl-units
      (let ((glew-include-path (jazz:quote-jazz-pathname "foreign/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "foreign/opengl/glew/lib/mac")))
        `((jazz.graphic.opengl.glew.foreign cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -framework OpenGL -lglew"))
          (jazz.graphic.opengl.glew.header cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -framework OpenGL -lglew"))))))
  (glfw
    (define jazz:opengl-units
      (let ((glew-include-path (jazz:quote-jazz-pathname "foreign/opengl/glew/include"))
            (glew-lib-path     (jazz:quote-jazz-pathname "foreign/opengl/glew/lib/mac")))
        `((jazz.graphic.opengl.glew.foreign cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -framework OpenGL -lglew"))
          (jazz.graphic.opengl.glew.header cc-options: ,(string-append "-I" glew-include-path) ld-options: ,(string-append "-L" glew-lib-path " -framework OpenGL -lglew"))))))
  (else
    (define jazz:opengl-units
      '())))


(define (jazz:build-opengl descriptor #!key (unit #f) (force? #f))
  (let ((unit-specs jazz:opengl-units))
    (jazz:custom-compile/build unit-specs pre-build: jazz:copy-opengl-files unit: unit force?: force?)
    (if (or (not unit) (not (assq unit unit-specs)))
        (jazz:build-product-descriptor descriptor unit: unit force?: force?))))


;;;
;;;; Register
;;;


(jazz:register-product 'jazz.graphic.opengl
  build: jazz:build-opengl))

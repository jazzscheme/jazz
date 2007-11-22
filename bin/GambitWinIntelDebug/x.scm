(begin
  (declare (block) (standard-bindings) (extended-bindings))
  (jazz.load-module 'jazz)
  (begin
    (define test.X
      (if (jazz.global-variable? 'test.X)
          (jazz.global-value 'test.X)
          (jazz.new-class
           jazz.Object-Class
           'test.X
           jazz.dialect.language.Object
           (%%list))))
    (define test.X:level (%%get-class-level test.X))
    (jazz.update-class test.X))
  (begin
    (define test.Y
      (if (jazz.global-variable? 'test.Y)
          (jazz.global-value 'test.Y)
          (jazz.new-class jazz.Object-Class 'test.Y test.X (%%list))))
    (define test.Y:level (%%get-class-level test.Y))
    (jazz.update-class test.Y))
  (jazz.define-generic (test.foo (test.X x) u a b))
  (jazz.define-specific (test.foo (test.X x) u a b) (+ a b))
  (jazz.define-specific (test.foo (test.Y x) u a b) (cons a b))
  (jazz.debug (test.foo (jazz.new0 test.X) #f 1 2))
  (jazz.debug (test.foo (jazz.new0 test.Y) #f 1 2))
  (jazz.module-loaded 'test))

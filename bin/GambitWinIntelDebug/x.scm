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
  (begin
    (define test.U
      (if (jazz.global-variable? 'test.U)
          (jazz.global-value 'test.U)
          (jazz.new-class
           jazz.Object-Class
           'test.U
           jazz.dialect.language.Object
           (%%list))))
    (define test.U:level (%%get-class-level test.U))
    (jazz.update-class test.U))
  (begin
    (define test.V
      (if (jazz.global-variable? 'test.V)
          (jazz.global-value 'test.V)
          (jazz.new-class jazz.Object-Class 'test.V test.U (%%list))))
    (define test.V:level (%%get-class-level test.V))
    (jazz.update-class test.V))
  (jazz.define-generic (test.foo (test.X x) (test.U u) a b))
  (jazz.define-specific (test.foo (test.X x) (test.U u) a b) (+ a b))
  (jazz.define-specific (test.foo (test.Y x) (test.U u) a b) (list a b))
  (jazz.define-generic (test.baz (test.X x) (test.U u) #!rest rest))
  (jazz.define-specific
   (test.baz (test.X x) (test.U u) #!rest rest)
   (apply + rest))
  (jazz.define-specific
   (test.baz (test.Y x) (test.U u) #!rest rest)
   (apply list rest))
  (jazz.module-loaded 'test))

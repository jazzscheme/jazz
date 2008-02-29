;;;
;;;; example
;;;


(c-package example


  ;;;
  ;;;; shape
  ;;;


  (c-class shape extends ()


    (c-external ("_wrap_delete_Shape" delete_Shape) void
      (self pointer))


    (c-external ("_wrap_Shape_x_set" Shape_x_set) void
      (self pointer)
      (x double))


    (c-method (set-x arg0 (obj shape))
      (Shape_x_set obj arg0))


    (c-external ("_wrap_Shape_x_get" Shape_x_get) double
      (self pointer))


    (c-method (get-x (obj shape))
      (Shape_x_get obj))


    (c-external ("_wrap_Shape_y_set" Shape_y_set) void
      (self pointer)
      (y double))


    (c-method (set-y arg0 (obj shape))
      (Shape_y_set obj arg0))


    (c-external ("_wrap_Shape_y_get" Shape_y_get) double
      (self pointer))


    (c-method (get-y (obj shape))
      (Shape_y_get obj))


    (c-external ("_wrap_Shape_move" Shape_move) void
      (self pointer)
      (dx double)
      (dy double))


    (c-method (move (obj shape) (self shape) (dx double) (dy double))
      (Shape_move obj self dx dy))


    (c-external ("_wrap_Shape_area" Shape_area) double
      (self pointer))


    (c-method (area (obj shape) (self shape))
      (Shape_area obj self))


    (c-external ("_wrap_Shape_perimeter" Shape_perimeter) double
      (self pointer))


    (c-method (perimeter (obj shape) (self shape))
      (Shape_perimeter obj self))

    (c-definition ("Shape_nshapes" Shape_nshapes)
      int))


  ;;;
  ;;;; circle
  ;;;


  (c-class circle extends (Shape)


    (c-external ("_wrap_new_Circle" new_Circle) pointer
      (r double))


    (c-external ("_wrap_Circle_area" Circle_area) double
      (self pointer))


    (c-method (area (obj circle) (self circle))
      (Circle_area obj self))


    (c-external ("_wrap_Circle_perimeter" Circle_perimeter) double
      (self pointer))


    (c-method (perimeter (obj circle) (self circle))
      (Circle_perimeter obj self))


    (c-external ("_wrap_delete_Circle" delete_Circle) void
      (self pointer)))


  ;;;
  ;;;; square
  ;;;


  (c-class square extends (Shape)


    (c-external ("_wrap_new_Square" new_Square) pointer
      (w double))


    (c-external ("_wrap_Square_area" Square_area) double
      (self pointer))


    (c-method (area (obj square) (self square))
      (Square_area obj self))


    (c-external ("_wrap_Square_perimeter" Square_perimeter) double
      (self pointer))


    (c-method (perimeter (obj square) (self square))
      (Square_perimeter obj self))


    (c-external ("_wrap_delete_Square" delete_Square) void
      (self pointer))))

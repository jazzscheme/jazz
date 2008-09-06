(package org.jazz
  
  (root "src")
  (install jazz.install)
  (products (core core.product)
            (jazz jazz.product)
            (platform jazz.product.platform)
            (all jazz.product.all)
            (test test.product))
  (export core.base
          core.class
          core.exception
          core.generic
          core.library
          core.module
          jazz
          jazz.build
          jazz.catalog
          jazz.console
          jazz.depot
          jazz.development
          jazz.groupware
          jazz.ide
          jazz.io
          jazz.jml
          jazz.language.c
          jazz.language.commonlisp
          jazz.language.jazz
          jazz.language.lisp
          jazz.language.scheme
          jazz.language.sql
          jazz.library
          jazz.platform
          jazz.profile
          jazz.recorder
          jazz.runtime
          jazz.system
          jazz.ui
          jazz.ui.clipboard
          jazz.ui.view
          jazz.ui.window
          jazz.utilities
          scheme))

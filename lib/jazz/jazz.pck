(package jazz
  
  (root "src")
  (install jazz.install)
  (products (core (module core.product))
            (jazz (module jazz.product) (dependencies core))
            (platform (module jazz.product.platform) (dependencies jazz))
            (all (module jazz.product.all) (dependencies platform))
            (test (module test.product))))

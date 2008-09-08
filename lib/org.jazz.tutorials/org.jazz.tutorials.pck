(package org.jazz.tutorials
  
  (root "src")
  (products (c4 (module jazz.tutorials.c4.C4-Product) (dependencies platform))
            (gomoku (module jazz.tutorials.gomoku.Gomoku-Product) (dependencies platform))
            (game (module jazz.tutorials.game.Game-Product) (dependencies jazz))))

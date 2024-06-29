# construct_reprex

    Code
      fun()
    Output
      # 1 ------------------------------------------------------
      
      delayedAssign(
        "y",
        value = c,
        eval.env = constructive::.env(
          "0x123456789",
          parents = c("0x123456789", "0x123456789", "0x123456789", "namespace:constructive")
        )
      )
      x <- 102
      
      z <- 3
      
      
      # 2 ------------------------------------------------------
      
      delayedAssign(
        "y",
        value = c,
        eval.env = constructive::.env(
          "0x123456789",
          parents = c("0x123456789", "0x123456789", "0x123456789", "namespace:constructive")
        )
      )
      x <- 102
      
      z <- 3
      
      
      # 3 ------------------------------------------------------
      
      delayedAssign(
        "y",
        value = c,
        eval.env = constructive::.env(
          "0x123456789",
          parents = c("0x123456789", "0x123456789", "0x123456789", "namespace:constructive")
        )
      )
      x <- 102
      
      z <- 3
      


# construct_reprex

    Code
      fun()
    Output
      # 1 ------------------------------------------------------
      
      z <- 3
      
      x <- 102
      
      delayedAssign(
        "y",
        value = c,
        eval.env = constructive::.env(
          "0x123456789",
          parents = c("0x123456789", "0x123456789", "0x123456789", "namespace:constructive")
        )
      )
      
      # 2 ------------------------------------------------------
      
      c <- 3
      
      a <- 101
      
      delayedAssign(
        "b",
        value = hello + world,
        eval.env = constructive::.env(
          "0x123456789",
          parents = c("0x123456789", "0x123456789", "0x123456789", "namespace:constructive")
        )
      )
      # inner2(a, c)
      
      # 3 ------------------------------------------------------
      
      hello <- 0
      
      # outer(100, hello + world)


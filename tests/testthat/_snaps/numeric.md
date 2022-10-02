# numeric

    Code
      construct(10000)
    Output
      10000
    Code
      construct(1e+05)
    Output
      1e+05
    Code
      construct(0.1)
    Output
      0.1
    Code
      construct(0.1)
    Output
      0.1
    Code
      construct(1.1e-15)
    Output
      1.1e-15
    Code
      construct(c(1, 2, 3), max_atomic = 0)
    Output
      numeric(0)
    Code
      construct(c(1, 2, 3), max_atomic = 2)
    Output
      c(1, 2, +1)
    Code
      construct(0.07)
    Output
      0.07
    Code
      construct(NA_real_)
    Output
      NA_real_
    Code
      construct(c(1, NA_real_))
    Output
      c(1, NA)


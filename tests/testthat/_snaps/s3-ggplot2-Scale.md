# multiplication works

    Code
      construct(scale_alpha())
    Condition
      Warning in `.Call()`:
      converting NULL pointer to R NULL
    Message
      ! The code built by {constructive} could not be evaluated.
    Output
      ggplot2::continuous_scale(
        aesthetics = "alpha",
        scale_name = "alpha_c",
        palette = (function(x) {
          rescale(x, range, c(0, 1))
        }) |>
          match.fun("environment<-")(constructive::env("0x000000000", parents = "namespace:scales"))
      )

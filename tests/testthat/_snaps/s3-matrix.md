# matrix

    Code
      construct(WorldPhones)
    Output
      matrix(
        c(
          45939, 60423, 64721, 68484, 71799, 76036, 79831, 21574, 29990, 32510, 35218,
          37598, 40341, 43173, 2876, 4708, 5230, 6662, 6856, 8220, 9053, 1815, 2568,
          2695, 2845, 3000, 3145, 3338, 1646, 2366, 2526, 2691, 2868, 3054, 3224, 89,
          1411, 1546, 1663, 1769, 1905, 2005, 555, 733, 773, 836, 911, 1008, 1076
        ),
        nrow = 7L,
        ncol = 7L,
        dimnames = list(
          c("1951", "1956", "1957", "1958", "1959", "1960", "1961"),
          c("N.Amer", "Europe", "Asia", "S.Amer", "Oceania", "Africa", "Mid.Amer")
        )
      )
    Code
      construct(matrix(1:9, 3))
    Output
      matrix(1:9, nrow = 3L, ncol = 3L)
    Code
      construct(matrix(1:9, 1))
    Output
      matrix(1:9, nrow = 1L, ncol = 9L)
    Code
      construct(matrix(1:9, 3), opts_matrix("array"))
    Output
      array(1:9, dim = c(3L, 3L))
    Code
      construct(matrix(1:9, 3), opts_matrix("next"))
    Output
      array(1:9, dim = c(3L, 3L))

# classed matrix

    Code
      construct(structure(matrix(1:9, 3), class = "a"))
    Output
      matrix(1:9, nrow = 3L, ncol = 3L) |>
        structure(class = "a")

# matrix with rbind and cbind

    Code
      construct(matrix(1:4, 2), opts_matrix("cbind"))
    Output
      cbind(1:2, 3:4)
    Code
      construct(matrix(1:4, 2, dimnames = list(c("a", "b"), c("c", "d"))),
      opts_matrix("cbind"))
    Output
      cbind(c = c(a = 1L, b = 2L), d = c(a = 3L, b = 4L))
    Code
      construct(matrix(1:4, 2, dimnames = list(c("a", "b"))), opts_matrix("cbind"))
    Output
      cbind(c(a = 1L, b = 2L), c(a = 3L, b = 4L))
    Code
      construct(matrix(1:4, 2, dimnames = list(NULL, c("c", "d"))), opts_matrix(
        "cbind"))
    Output
      cbind(c = 1:2, d = 3:4)
    Code
      construct(matrix(1:4, 2), opts_matrix("rbind"))
    Output
      rbind(c(1L, 3L), c(2L, 4L))
    Code
      construct(matrix(1:4, 2, dimnames = list(c("a", "b"), c("c", "d"))),
      opts_matrix("rbind"))
    Output
      rbind(a = c(c = 1L, d = 3L), b = c(c = 2L, d = 4L))
    Code
      construct(matrix(1:4, 2, dimnames = list(c("a", "b"))), opts_matrix("rbind"))
    Output
      rbind(a = c(1L, 3L), b = c(2L, 4L))
    Code
      construct(matrix(1:4, 2, dimnames = list(NULL, c("c", "d"))), opts_matrix(
        "rbind"))
    Output
      rbind(c(c = 1L, d = 3L), c(c = 2L, d = 4L))


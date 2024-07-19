test_that("R6Class", {
  expect_snapshot({
    # example from `?R6::R6Class`
    Queue <- R6::R6Class(
      "Queue",
      public = list(
        initialize = function(...) {
          for (item in list(...)) {
            self$add(item)
          }
        },
        add = function(x) {
          private$queue <- c(private$queue, list(x))
          invisible(self)
        },
        remove = function() {
          if (private$length() == 0) return(NULL)
          # Can use private$queue for explicit access
          head <- private$queue[[1]]
          private$queue <- private$queue[-1]
          head
        }
      ),
      private = list(
        queue = list(),
        length = function() base::length(private$queue)
      )
    )
    # check = FALSE because of environments
    construct(Queue, check = FALSE)
  })
})

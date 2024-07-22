test_that("multiplication works", {
  expect_snapshot({
    # example from `?R6::R6Class`
    Queue1 <- R6::R6Class(
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
    construct(Queue1$new(1, 2), check = FALSE)
    construct(Queue1$new(1, 2), check = FALSE, one_liner = TRUE)

    # without initialize() method
    Queue2 <- R6::R6Class(
      "Queue",
      public = list(
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
    construct(Queue2$new(), check = FALSE)
    construct(Queue2$new(), check = FALSE, one_liner = TRUE)
  })
})

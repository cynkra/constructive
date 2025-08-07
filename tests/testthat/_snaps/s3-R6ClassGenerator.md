# R6Class

    Code
      Queue <- R6::R6Class("Queue", public = list(initialize = function(...) {
        for (item in list(...)) {
          self$add(item)
        }
      }, add = function(x) {
        private$queue <- c(private$queue, list(x))
        invisible(self)
      }, remove = function() {
        if (private$length() == 0) return(NULL)
        head <- private$queue[[1]]
        private$queue <- private$queue[-1]
        head
      }), private = list(queue = list(), length = function() base::length(private$
        queue)))
      construct(Queue, check = FALSE)
    Output
      R6::R6Class(
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
            head <- private$queue[[1]]
            private$queue <- private$queue[-1]
            head
          }
        ),
        private = list(queue = list(), length = function() base::length(private$queue)),
        parent_env = constructive::.env(
          "0x123456789",
          parents = c("0x123456789", "0x123456789", "namespace:constructive")
        )
      ) |>
        structure(name = "Queue_generator")


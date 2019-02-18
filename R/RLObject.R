#'@export
RLObject <- R6::R6Class("RLObject",
                        public = list(
                          initialize = function(shape, type, description){
                            private$.id <- uuid::UUIDgenerate();
                          },
                          print = function(...) {
                            cat("RLObject: \n")
                            cat("  id: ", private$.id, "\n", sep = "")
                            invisible(self);
                          }
                        ),
                        private = list(
                          # the uuid of the state
                          .id = NULL
                        ),
                        active = list(
                          id = function(value) {
                            if (missing(value)) {
                              private$.id
                            } else {
                              stop("`$id` cannot be assigned", call. = T)
                            }
                          }
                        )
);

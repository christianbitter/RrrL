#'@export
RLAgent <- R6::R6Class("RLAgent",
                       inherit = RLObject,
                       public = list(
                         initialize = function(name) {
                           super$initialize()
                           
                           if (!missing(name) && !is.null(name)) {
                             private$.name <- name;
                           }
                         },
                         select_action = function(){
                           
                         }
                       ),
                       private = list(
                         .name = NULL
                       ),
                       active = list(
                         name = function(value) {
                           if (missing(value)) {
                             private$.name
                           } else {
                             stop("`$name` cannot be assigned", call. = FALSE)
                           }
                         }
                       )
);
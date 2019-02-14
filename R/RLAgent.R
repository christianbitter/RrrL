#'@export
RLAgent <- R6::R6Class("RLAgent",
                       public = list(
                         initialize = function(name){
                           if (!missing(name) && !is.null(name)) {
                             private$.name <- name;
                           }
                         },
                         select_action = function(){
                           
                         }
                       ),
                       private = list(
                         .id = uuid::UUIDgenerate(),
                         .name = NULL
                       ),
                       active = list(
                         name = function(value) {
                           if (missing(value)) {
                             private$.name
                           } else {
                             stop("`$name` cannot be assigned", call. = FALSE)
                           }
                         },
                         id = function(value) {
                           if (missing(value)) {
                             private$.id
                           } else {
                             stop("`$id` cannot be assigned", call. = FALSE)
                           }
                         }
                       )
);
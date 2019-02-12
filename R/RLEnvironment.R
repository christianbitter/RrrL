RLEnvironment <- R6::R6Class("RLEnvironment",
                             public = list(
                               initialize = function(){
                                 
                               }
                             ),
                             private = list(
                               .state_space = NULL,
                               .action_space = NULL
                             ),
                             active = list(
                               state_space = function(value) {
                                 if (missing(value)) {
                                   private$.state_space
                                 } else {
                                   stop("`$state_space` cannot be assigned", call. = FALSE)
                                 }
                               },
                               action_space = function(value) {
                                 if (missing(value)) {
                                   private$.action_space
                                 } else {
                                   stop("`$action_space` cannot be assigned", call. = FALSE)
                                 }
                               }
                             )
);
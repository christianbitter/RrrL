RLEnvironment <- R6::R6Class("RLEnvironment",
                             inherit = RLObject,
                             public = list(
                               initialize = function(){
                                 super$initialize()
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
                                   stop("`$state_space` cannot be assigned", call. = F)
                                 }
                               },
                               action_space = function(value) {
                                 if (missing(value)) {
                                   private$.action_space
                                 } else {
                                   stop("`$action_space` cannot be assigned", call. = F)
                                 }
                               }
                             )
);
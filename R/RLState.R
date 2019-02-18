#'@export
StateType <- function(){
  return(list(DISCRETE = "Discrete", 
              CONTINUOUS = "Continuous"));
}

validate_type <- function(types) {
  if (missing(types)) stop("types missing", call. = T);
  result <- T;
  n <- tolower(names(StateType()));
  for (t in types) {
    if (!(tolower(t) %in% n)) {
      result <- F;
      break;
    }
  }
  
  return(result);
}

#'@export
RLStateSpaceDescriptor <- R6::R6Class("RLStateSpaceDescriptor",
                       public = list(
                         initialize = function(shape, type, description) {
                           super$initialize()
                           
                           if (missing(shape)) stop("state space shape not provided", call. = T);
                           stopifnot(length(shape) > 0);
                           
                           private$.shape <- shape;
                           
                           if (missing(type)) stop("state space type not provided", call. = T);
                           stopifnot(length(type) > 0, dim(type) == dim(shape), validate_type(type))
                           private$.type <- type;
                           
                           if (!missing(description)) {
                             stopifnot(is.character(description), length(description) > 0);
                             private$.description <- description;
                           }
                         },
                         getExample = function(){
                           stop("getExample not implemented", call. = T)
                         }
                       ),
                       active = list(
                         shape = function(value) {
                           if (missing(value)) {
                             private$.shape
                           } else {
                             stop("`$shape` cannot be assigned", call. = T)
                           }
                         },
                         type = function(value) {
                           if (missing(value)) {
                             private$.type
                           } else {
                             stop("`$type` cannot be assigned", call. = T)
                           }
                         },
                         description = function(value) {
                           if (missing(value)) {
                             private$.description
                           } else {
                             stop("`$description` cannot be assigned", call. = T)
                           }
                         }                        
                       ),
                       private = list(
                         # the dimensionality or shape
                         .shape = NULL,
                         # the primitive type
                         .type = NULL,
                         # a human readable description
                         .description = NULL
                       )
);

#'@name
#'@title
#'@author
#'@description
#'@param k
#'The number of bandit arms in the k-armed bandit setting
#'@return
#'The constructed state space descriptor
#'@export
KArmedBanditStateSpaceDescriptor <- function(k){
  shape <- c(k);
  type  <- c("DISCRETE");
  dim(shape) <- c(1);
  dim(type) <- c(1);
  desc  <- "The K-Armed Bandit State Space is a one-dimensional discrete space. 
            Where we can be in any of the picked arm <J> states.";
  return(RLStateSpaceDescriptor$new(shape = shape, type = type, description = desc));
}
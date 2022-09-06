#' Replace categorical features with integers
#' Utility function to replace categorical features with integer representation.
#' @noRd
categorical_features_to_int <- function(x, cat_indices){
  for(i in cat_indices){
    x[[i]] <- as.integer(x[[i]]) -1
  }
  x
}

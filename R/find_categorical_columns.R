#' Retrieve the indices of categorical (factor) columns
#' Utility function to help identify factors in data.frame.
#' Does only identify the columns, nothing else.
#' @noRd
find_factor_column_idx <- function(data){
  return_value <- c()
  for(i in seq_along(data)){
    if(is.factor(x=data[[i]])){
      return_value <- c(return_value, i)
    }
  }
  return(return_value)
}

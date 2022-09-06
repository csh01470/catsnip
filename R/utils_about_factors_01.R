#' Retrieve the indices of categorical (factor) columns
#' Utility function to help identify factors in data.frame.
#' Does only identify the columns, nothing else.
#' @noRd
categorical_columns <- function(x){
  categorical_cols <- NULL
  for(i in seq_along(x)){
    if(is.factor(x[[i]])){
      categorical_cols <- c(categorical_cols, i)
    }
  }
  categorical_cols
}

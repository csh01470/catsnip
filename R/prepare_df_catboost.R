#' @noRd
prepare_df_catboost <- function(x, y = NULL, categorical_cols= NULL){
  # auto detect the categorical columns from data.frame
  # Not strictly necessary but good form.
  if(is.null(categorical_cols)){
    categorical_cols <- categorical_columns(x)
  }

  # catboost uses 0-indexed feature cols
  if(!is.null(categorical_cols)){
    categorical_cols <- categorical_cols-1
  }
  if(is.null(y)){
    return(x)
  }

  catboost.load_pool(data = x, label = y, cat_features = categorical_cols)
}

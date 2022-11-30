#' @noRd
prepare_df_catboost <- function(x, y=NULL, categorical_cols=NULL){
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

  main_args <- list(
    data         = x,
    label        = y,
    cat_features = categorical_cols
  )
  call <- parsnip::make_call(fun="catboost.load_pool", ns="catboost", args=main_args)
  rlang::eval_tidy(expr=call, env=rlang::current_env())
}

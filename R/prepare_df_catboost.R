#' @noRd
prepare_df_catboost <- function(x, y=NULL, categorical_col_idx=NULL){
  if(is.null(x=categorical_col_idx)){
    categorical_col_idx <- find_factor_column_idx(x)
    categorical_col_idx <- categorical_col_idx-1
  }
  if(is.null(x=y)){
    if(is.data.frame(x=x)){
      main_args <- list(
        data         = x,
        label        = NULL,
        cat_features = NULL
      )
    }else{
      main_args <- list(
        data         = x,
        label        = NULL,
        cat_features = categorical_col_idx
      )
    }
  }else{
    if(is.factor(x=y)){
      y <- as.numeric(x=y)-1
    }
    if(is.data.frame(x=x)){
      main_args <- list(
        data         = x,
        label        = y,
        cat_features = NULL
      )
    }else{
      main_args <- list(
        data         = x,
        label        = y,
        cat_features = categorical_col_idx
      )
    }
  }
  call <- parsnip::make_call(fun="catboost.load_pool", ns="catboost", args=main_args)
  return_value <- rlang::eval_tidy(expr=call, env=rlang::current_env())
  return(return_value)
}

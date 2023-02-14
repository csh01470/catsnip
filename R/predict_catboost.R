#' @noRd
predict.catboost.Model <- function(object, new_data, type="RawFormulaVal", categorical_col_idx=NULL, ...){
  if(!inherits(x=new_data, what="catboost.Pool")){
    new_data <- prepare_df_catboost(x=new_data, y=NULL, categorical_col_idx=categorical_col_idx)
  }
  prediction_type <- switch(
    type,
    "raw"     = "RawFormulaVal",
    "numeric" = "RawFormulaVal",
    "class"   = "Class",
    "prob"    = "Probability",
    type
  )

  main_args <- list(
    model           = object,
    pool            = new_data,
    prediction_type = prediction_type,
    ...
  )
  call <- parsnip::make_call(fun="catboost.predict", ns="catboost", args=main_args)
  return_value <- rlang::eval_tidy(expr=call, env=rlang::current_env())
  return(return_value)
}

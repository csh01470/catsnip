#' @export
#' @noRd
predict.catboost.Model <- function(object, new_data, type="RawFormulaVal", categorical_cols=NULL, ...){
  if(!inherits(new_data, "catboost.Pool")){
    d <- prepare_df_catboost(x=new_data, y=NULL, categorical_cols=categorical_cols)
    # new_data <- catboost::catboost.load_pool(d, cat_features=categorical_cols)
    #I_NEED_MORE_STUDY
    FORM <- "new_data <- catboost::catboost.load_pool(d, cat_features=categorical_cols)"
    eval(expr=parse(text=FORM))
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
    object,
    new_data,
    prediction_type=prediction_type,
    ...
  )
  call <- parsnip::make_call(fun="catboost.predict", ns="catboost", main_args)
  rlang::eval_tidy(expr=call, env=rlang::current_env())
}

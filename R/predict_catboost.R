#' @export
#' @noRd
predict.catboost.Model <- function(object, new_data, type="RawFormulaVal", categorical_cols=NULL, ...){
  if(!inherits(new_data, "catboost.Pool")){
    d <- prepare_df_catboost(new_data, categorical_cols=categorical_cols)
    new_data <- catboost.load_pool(d, cat_features=categorical_cols)
  }

  prediction_type <- switch(
    type,
    "raw"     = "RawFormulaVal",
    "numeric" = "RawFormulaVal",
    "class"   = "Class",
    "prob"    = "Probability",
    type
  )

  catboost.predict(object, new_data, prediction_type=prediction_type, ...)
}

globalVariables(names=c(
  "predict.catboost_Model", "catboost.load_pool", "catboost.predict"
))

#' @title Model predictions across many sub-models
#' @description For some models, predictions can be made on sub-models in the model object.
#'
#' @param object A model_fit object.
#' @param ... Optional arguments to pass to predict.model_fit(type = "raw") such as type.
#' @param new_data A rectangular data object, such as a data frame.
#' @param type A single character value or NULL. Possible values are "numeric", "class", "prob", "conf_int", "pred_int", "quantile", or "raw". When NULL, predict() will choose an appropriate value based on the model's mode.
#' @param trees An integer vector for the number of trees in the ensemble.
#' @param categorical_col_idx indices of categorical columns, when NULL (default) factor columns are automatically detected.
#'
#' @importFrom purrr map_df
#' @importFrom parsnip multi_predict
#' @export
multi_predict._catboost.Model <- function(object, new_data, type=NULL, trees=NULL,
                                          categorical_col_idx=NULL, ...){
  if(any(names(rlang::enquos(...))=="newdata")){
    rlang::abort("Did you mean to use `new_data` instead of `newdata`?")
  }
  if(is.null(trees)){
    trees <- object[["fit"]][["tree_count"]]
  }
  trees <- sort(trees)

  if(is.null(type)){
    if(object[["spec"]][["mode"]]=="classification"){
      type <- "Class"
    }else{
      type <- "RawFormulaVal"
    }
  }else{
    type <- switch(
      type,
      "raw"     = "RawFormulaVal",
      "numeric" = "RawFormulaVal",
      "class"   = "Class",
      "prob"    = "Probability",
      type
    )
  }

  res <- map_df(trees, catboost_by_tree, object=object, new_data=new_data,
                type=type, categorical_col_idx=categorical_col_idx, ...)
  res <- dplyr::arrange(res, .row, trees)
  res <- split(res[, -1], res$.row)
  names(res) <- NULL

  return_value <- tibble::tibble(.pred = res)
  return(return_value)
}

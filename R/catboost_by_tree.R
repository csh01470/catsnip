#' @noRd
catboost_by_tree <- function(tree, object, new_data, type, categorical_col_idx=NULL, ...){
  d <- prepare_df_catboost(x=new_data, y=NULL, categorical_col_idx=categorical_col_idx)
  pred <- predict.catboost.Model(object$fit, d, ntree_end=tree, type=type,
                                 categorical_col_idx=categorical_col_idx, ...)
  if(object[["spec"]][["mode"]]=="regression"){
    pred <- tibble::tibble(.pred = pred)
    nms <- names(x=pred)
  }else{
    if(type=="Class"){
      pred <- object[["spec"]][["method"]][["pred"]][["class"]][["post"]](pred, object)
      pred <- tibble::tibble(.pred_class=factor(x=pred, levels=object$lvl))
    }else{
      pred <- object[["spec"]][["method"]][["pred"]][["prob"]][["post"]](pred, object)
      pred <- tibble::as_tibble(pred)
      names(pred) <- paste0(".pred_", names(x=pred))
    }
    nms <- names(x=pred)
  }
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(x=new_data)

  return_value <- pred[, c(".row", "trees", nms)]
  return(return_value)
}

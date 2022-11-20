#' @noRd
catboost_by_tree <- function(tree, object, new_data, type, categorical_cols=NULL, ...){
  d <- prepare_df_catboost(new_data, categorical_cols=categorical_cols)
  pred <- predict.catboost.Model(object$fit, d, ntree_end=tree, type=type,
                                 categorical_cols=categorical_cols, ...)
  # switch based on prediction type
  if(object$spec$mode == "regression"){
    pred <- tibble::tibble(.pred = pred)
    nms <- names(pred)
  } else{
    if(type == "Class"){
      pred <- object$spec$method$pred$class$post(pred, object)
      pred <- tibble::tibble(.pred_class = factor(pred, levels=object$lvl))
    } else{
      pred <- object$spec$method$pred$prob$post(pred, object)
      pred <- tibble::as_tibble(pred)
      names(pred) <- paste0(".pred_", names(pred))
    }
    nms <- names(pred)
  }
  pred[["trees"]] <- tree
  pred[[".row"]] <- 1:nrow(new_data)
  pred[, c(".row", "trees", nms)]
}

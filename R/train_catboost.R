#' @title Boosted trees via catboost
#' @description `catboost_train` is a wrapper for `catboost` tree-based models
#'               where all of the model arguments are in the main function.
#'
#' @param x A data frame or matrix of predictors
#' @param y A vector (factor or numeric) or matrix (numeric) of outcome data.
#' @param depth An integer for the maximum depth of the tree.
#' @param iterations An integer for the number of boosting iterations.
#' @param learning_rate A numeric value between zero and one to control the learning rate.
#' @param rsm Subsampling proportion of columns.
#' @param min_data_in_leaf A numeric value for the minimum sum of instances needed
#'  in a child to continue to split.
#' @param subsample Subsampling proportion of rows.
#' @param early_stopping_rounds overfitting detector type to Iter and stops the training.
#' @param categorical_col_idx indices of categorical columns, when NULL (default) factor columns are automatically detected
#' @param ... Other options to pass to `catboost.train`.
#'
#' @return A fitted `catboost.Model` object.
#'
#' @export
train_catboost <- function(x, y, depth=6, iterations=1000, learning_rate=NULL, rsm=1,
                           min_data_in_leaf=1, subsample=1, early_stopping_rounds=NULL,
                           categorical_col_idx=NULL, ...){

  others <- list(...)
  d <- prepare_df_catboost(x=x, y=y, categorical_col_idx=categorical_col_idx)

  learning_rate <- max(learning_rate, 1e-6)

  if(others[["task_type"]]=="GPU"){
    rsm <- NULL
  }else{
    if(!is.null(x=rsm)){
      rsm <- rsm/ncol(x=x)
      if(rsm>1){
        rsm <- 1
      }
    }
  }

  if(subsample>1){
    subsample <- 1
  }

  arg_list <- list(
    iterations            = iterations,
    learning_rate         = learning_rate,
    depth                 = depth,
    rsm                   = rsm,
    min_data_in_leaf      = min_data_in_leaf,
    subsample             = subsample,
    early_stopping_rounds = early_stopping_rounds
  )

  names(x=others)[names(x=others)%in%c("objective")] <- "loss_function"
  if(!any(names(x=others)%in%c("loss_function"))){
    if(is.numeric(x=y)){
      arg_list[["loss_function"]] <- "RMSE"
    }else{
      lvl <- levels(x=y)
      y <- as.numeric(x=y)-1
      if(length(x=lvl)==2){
        arg_list[["loss_function"]] <- "Logloss"
      }else{
        arg_list[["loss_function"]] <- "MultiClass"
      }
    }
  }

  # override or add some other args
  others <- others[!(names(x=others)%in%c("learn_pool", "test_pool", names(x=arg_list)))]

  if(is.null(x=others[["logging_level"]])){
    others[["logging_level"]] <- "Silent"
  }
  if(is.null(x=others[["bootstrap_type"]])){
    others[["bootstrap_type"]] <- "Bernoulli"
  } # subsample as is
  if(is.null(x=others[["sampling_frequency"]])){
    others[["sampling_frequency"]] <- "PerTree"
  } # subsample as is

  # artificial alias for thread_count (for match xgboost and lightgbm)
  if(is.null(x=others[["thread_count"]])&is.null(x=others[["nthread"]])){
    others[["thread_count"]] <- 1L # parallelism should be explicitly specified by the user
  }else{
    others[["thread_count"]] <- ifelse(test=(!is.null(x=others[["thread_count"]])),
                                       yes=(others[["thread_count"]]),
                                       no=(others[["nthread"]]))
    others[["nthread"]] <- NULL
  }

  arg_list <- purrr::compact(.x=c(arg_list, others))
  main_args <- list(
    learn_pool = quote(expr=d),
    params     = arg_list
  )

  call <- parsnip::make_call(fun="catboost.train", ns="catboost", args=main_args)
  return_value <- rlang::eval_tidy(expr=call, env=rlang::current_env())
  return(return_value)
}

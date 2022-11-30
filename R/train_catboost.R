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
#' @param categorical_cols indices of categorical columns, when NULL (default) factor columns are automatically detected
#' @param ... Other options to pass to `catboost.train`.
#' @return A fitted `catboost.Model` object.
#' @keywords internal
#' @export
train_catboost <- function(x, y, depth=6, iterations=1000, learning_rate=NULL, rsm=1, min_data_in_leaf=1,
                           subsample=1, categorical_cols=NULL, ...){
  others <- list(...)

  # learning rate --------------------
  learning_rate <- max(learning_rate, 1e-6)

  # rsm ------------------------------
  if(!is.null(rsm)){
    rsm <- rsm/ncol(x)
  }else if(rsm>1){
    rsm <- 1
  }

  # subsample -----------------------
  if(subsample>1){
    subsample <- 1
  }

  arg_list <- list(
    iterations       = iterations,
    learning_rate    = learning_rate,
    depth            = depth,
    rsm              = rsm,
    min_data_in_leaf = min_data_in_leaf,
    subsample        = subsample
  )

  # loss -------------------------
  # objective accepted as an alias for loss_function
  names(others)[names(others) %in% "objective"] <- "loss_function"

  if(!any(names(others)%in%c("loss_function"))){
    if(is.numeric(y)){
      arg_list$loss_function <- "RMSE"
    }else{
      lvl <- levels(y)
      y <- as.numeric(y) - 1
      if(length(lvl)==2){
        arg_list$loss_function <- "Logloss"
      }else{
        arg_list$loss_function <- "MultiClass"
      }
    }
  }

  # train ------------------------
  d <- prepare_df_catboost(x, y=y, categorical_cols=categorical_cols)

  # override or add some other args
  others <- others[!(names(others) %in% c("learn_pool", "test_pool", names(arg_list)))]

  if(is.null(others$logging_level)){
    others$logging_level <- "Silent"
  }
  if(is.null(others$bootstrap_type)){
    others$bootstrap_type <- "Bernoulli"
  } # subsample as is
  if(is.null(others$sampling_frequency)){
    others$sampling_frequency <- "PerTree"
  } # subsample as is

  # artificial alias for thread_count (for match xgboost and lightgbm)
  if(is.null(others$thread_count) & is.null(others$nthread)){
    others$thread_count <- 1L # parallelism should be explicitly specified by the user
  } else{
    others$thread_count <- ifelse(!is.null(others$thread_count), others$thread_count, others$nthread)
    others$nthread <- NULL
  }

  arg_list <- purrr::compact(c(arg_list, others))
  main_args <- list(
    learn_pool = quote(d),
    params     = arg_list
  )

  call <- parsnip::make_call(fun="catboost.train", ns="catboost", main_args)
  rlang::eval_tidy(expr=call, env=rlang::current_env())
}

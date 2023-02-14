#' @title Wrapper to add `catboost` engine to the `parsnip` format
#' @description Attach the `catboost` package to `boost_tree()` Function.
#'
#' @usage add_boost_tree_catboost()
#'
#' @export
add_boost_tree_catboost <- function(){
  parsnip::set_model_engine(model="boost_tree", mode="regression", eng="catboost")
  parsnip::set_model_engine(model="boost_tree", mode="classification", eng="catboost")
  parsnip::set_dependency(model="boost_tree", eng="catboost", pkg="catboost")
  parsnip::set_dependency(model="boost_tree", eng="catboost", pkg="catsnip")

  parsnip::set_encoding(
    model   = "boost_tree",
    mode    = "regression",
    eng     = "catboost",
    options = list(predictor_indicators = "none",
                   compute_intercept    = FALSE,
                   remove_intercept     = FALSE,
                   allow_sparse_x       = FALSE)
  )
  parsnip::set_encoding(
    model   = "boost_tree",
    mode    = "classification",
    eng     = "catboost",
    options = list(predictor_indicators = "none",
                   compute_intercept    = FALSE,
                   remove_intercept     = FALSE,
                   allow_sparse_x       = FALSE)
  )

  parsnip::set_fit(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "regression",
    value = list(interface = "data.frame",
                 protect   = c("x", "y"),
                 func      = c(pkg="catsnip", fun="train_catboost"),
                 defaults  = list(allow_writing_files=FALSE,
                                  task_type="CPU"))
  )
  parsnip::set_fit(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "classification",
    value = list(interface = "data.frame",
                 protect   = c("x", "y"),
                 func      = c(pkg="catsnip", fun="train_catboost"),
                 defaults  = list(allow_writing_files=FALSE,
                                  task_type="CPU"))
  )

  parsnip::set_pred(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "regression",
    type  = "numeric",
    value = list(pre  = NULL,
                 post = NULL,
                 func = c(fun = "predict"),
                 args = list(object = quote(expr=object[["fit"]]), new_data=quote(expr=new_data)))
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "regression",
    type  = "raw",
    value = list(pre  = NULL,
                 post = NULL,
                 func = c(fun = "predict"),
                 args = list(object = quote(expr=object[["fit"]]), new_data=quote(expr=new_data)))
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "classification",
    type  = "class",
    value = list(pre  = NULL,
                 func = c(pkg = NULL, fun="predict"),
                 args = list(object=quote(expr=object[["fit"]]), new_data=quote(expr=new_data)),
                 post = function(x, object){
                   if(is.vector(x=x)){
                     x <- ifelse(test=(x>=0.5), yes=(object[["lvl"]][[2]]), no=(object[["lvl"]][[1]]))
                   }else{
                     x <- object[["lvl"]][apply(X=x, MARGIN=1, FUN=which.max)]
                   }
                   return(x)
                 })
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "classification",
    type  = "prob",
    value = list(pre  = NULL,
                 func = c(pkg=NULL, fun="predict"),
                 args = list(object=quote(expr=object[["fit"]]), new_data=quote(new_data), type="prob"),
                 post = function(x, object){
                   if(is.vector(x=x)){
                     x <- tibble::tibble(v1=(1-x),v2 = x)
                   }else{
                     x <- tibble::as_tibble(x, .name_repair=make.names)
                   }
                   colnames(x=x) <- object[["lvl"]]
                   return(x)
                 })
  )
  parsnip::set_pred(
    model = "boost_tree",
    eng   = "catboost",
    mode  = "classification",
    type  = "raw",
    value = list(pre  = NULL,
                 post = NULL,
                 func = c(fun="predict"),
                 args = list(object=quote(object[["fit"]]), new_data=quote(new_data)))
  )

  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "tree_depth",
    original     = "depth",
    has_submodel = FALSE,
    func         = list(pkg="dials", fun="tree_depth")
  )
  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "trees",
    original     = "iterations",
    has_submodel = TRUE,
    func         = list(pkg="dials", fun="trees")
  )
  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "learn_rate",
    original     = "learning_rate",
    has_submodel = FALSE,
    func         = list(pkg="dials", fun="learn_rate")
  )
  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "mtry",
    original     = "rsm",
    has_submodel = FALSE,
    func         = list(pkg="dials", fun="mtry")
  )
  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "min_n",
    original     = "min_data_in_leaf",
    has_submodel = FALSE,
    func         = list(pkg="dials", fun="min_n")
  )
  # parsnip::set_model_arg(
  #   model        = "boost_tree",
  #   eng          = "catboost",
  #   parsnip      = "loss_reduction",
  #   original     = "gamma", # There is no such parameter in catboost
  #   has_submodel = FALSE,
  #   func         = list(pkg="dials", fun="loss_reduction")
  # )
  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "sample_size",
    original     = "subsample",
    has_submodel = FALSE,
    func         = list(pkg="dials", fun="sample_prop")
  )
  parsnip::set_model_arg(
    model        = "boost_tree",
    eng          = "catboost",
    parsnip      = "stop_iter",
    original     = "early_stopping_rounds",
    has_submodel = FALSE,
    func         = list(pkg="dials", fun="stop_iter")
  )
}

#' @noRd
.onLoad <- function(libname, pkgname){
  if(!"catboost" %in% parsnip::get_model_env()$boost_tree$engine){
    add_boost_tree_catboost()
  }
}

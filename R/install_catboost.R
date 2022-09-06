#' @title Installation of Catboost Packages
#' @description Install `Catboost` Package
#'
#' @export
#' @importFrom devtools install_url
#' @importFrom utils installed.packages
install_catboost <- function(){
  package_list <- installed.packages()[,1]
  if(!any(grepl(x=package_list, pattern="catboost"))){
    devtools::install_url(
      url="https://github.com/catboost/catboost/releases/download/v1.0.6/catboost-R-Windows-1.0.6.tgz",
      build_opts=c("--no-resave-data"),
      build_vignettes=TRUE
    )
  } else{
    cat("* Catboost package is already installed.")
  }
}

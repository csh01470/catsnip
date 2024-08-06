#' @title Installation of Catboost Packages
#' @description Install `Catboost` Package By URL.
#'
#' @param version catboost version that you want to install.
#'
#' @importFrom devtools install_url
#' @importFrom utils installed.packages
#' @export
install_catboost <- function(version="1.2.5"){
  if(identical(x=tolower(x=Sys.info()[["sysname"]]), y="darwin")){
    USER_OS <- "mac"
  }else if(identical(x=.Platform$OS.type, y="windows")){
    USER_OS <- "windows"
  }else{
    USER_OS <- "linux"
  }
  PKG_LIST <- installed.packages()[, 1]
  if(!any(grepl(x=PKG_LIST, pattern="catboost"))){
    if(USER_OS=="windows"){
      devtools::install_url(
        url=paste0("https://github.com/catboost/catboost/releases/download/v",
                   version,
                   "/catboost-R-Windows-",
                   version,
                   ".tgz"),
        build_opts=c("--no-resave-data"),
        build_vignettes=TRUE
      )
    }else if(USER_OS=="mac"){
      devtools::install_url(
        url=paste0("https://github.com/catboost/catboost/releases/download/v",
                   version,
                   "/catboost-R-Darwin-",
                   version,
                   ".tgz"),
        INSTALL_opts=c("--no-multiarch", "--no-test-load", "--no-staged-install"),
        build_vignettes=TRUE
      )
    }else{
      devtools::install_url(
        url=paste0("https://github.com/catboost/catboost/releases/download/v",
                   version,
                   "/catboost-R-Linux-",
                   version,
                   ".tgz"),
        build_opts=c("--no-resave-data"),
        build_vignettes=TRUE
      )
    }
  }else{
    cat(">> Catboost package is already installed.")
  }
}


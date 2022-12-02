#' Opens and Runs one/ multiple External R-Scripts on the current script.
#'
#' The function help to execute/run one or multiple R-scripts from a specified folder in the current working directory.
#'
#' By default echo is False(F), but can be set to true(F) to see each line of code in the external script execute.
#'
#'
#' @export
#' @param  rscript_folder a folder where R-scripts are stored.
#' @param  echo echo the external scripts when executing.
#' @param ...  any other function


# ------ Run External Scripts

run_multiple_rscripts <- function(rscript_folder,echo = F,...){
  if(!require(here)){install.packages(here); library(here)}

  lstRscript = dir(path = here(rscript_folder),pattern = "*.R")
  for (i in 1:length(lstRscript)) {
    source(file = here(rscript_folder, dir(path = here(rscript_folder),pattern = "*.R")[i]),echo = echo)
  }
  rm(list = ls())
}


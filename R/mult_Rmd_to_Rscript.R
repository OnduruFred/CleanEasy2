#' Converts one or more multiple Rmarkdown into R scripts.
#'
#' The function help to convert multiple Rmarkdown into R scripts and save the results of the scripts in the folder given.
#' Specifying the folder location works with here package. which help to obtain current working directory
#'
#' @export
#' @param  Rmd_input_folder a folder where Rmarkdown scripts are stored.
#' @param  Rscript_output_folder a folder where Produced R-scripts should be stored.
#' @param ...  any other function

#----- CONVERT Rmarkdown to R-Scripts
mult_Rmd_to_Rscript <- function(Rmd_input_folder,Rscript_output_folder,...){
  if(!require(here)){install.packages(here); library(here)}
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  lstrmd <- dir(path = here(Rmd_input_folder),pattern = "*.Rmd")
  for (i in 1:length(lstrmd)) {
    knitr::purl(input = here(Rmd_input_folder,lstrmd[i]),
                output = here(Rscript_output_folder,str_replace(lstrmd[i],"Rmd","R")),
                documentation = 2)
  }
}

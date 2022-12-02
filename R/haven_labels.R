#' Extract Haven labels.
#'
#' Extract haven labels from Stata and SPSS labelled data set/variable and returns a tibble with values and there labels
#'
#'
#' @export
#' @param  x haven labelled variable <dbl+lbl>.
#' @param  ... other functions


# -----  Extract haven levels and labels
haven_labels <- function(x,...){
  if(!require(labelled)){install.packages(labelled); library(labelled)}
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  havn_lab = as.matrix(val_labels(x))
  levels = as.vector(havn_lab[,1])
  labels = rownames(havn_lab)
  return(rownames_to_column(as.data.frame(levels,labels),var = "labels"))
}

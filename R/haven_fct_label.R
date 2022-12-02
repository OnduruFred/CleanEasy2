#' Extract Haven labels and convert the variable to factor labelled variable.
#'
#' The function help to extract haven labels from Stata and SPSS labelled data set/variable, and converts the variable
#' into factor labeled variable ready for analysis. ie It performs both variable label and value/ levels labels for
#' the new factor variable,
#'
#'
#' @export
#' @param  x haven labelled variable <dbl+lbl>.
#' @param  ... other functions

# -----  Extract haven levels and labels
haven_fct_label <- function(x,...){
  if(!require(labelled)){install.packages(labelled); library(labelled)}
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  havn_lab = as.matrix(val_labels(x))
  levels = as.vector(havn_lab[,1])
  labels = rownames(havn_lab)
  variable = factor(x,
                     levels = levels,
                     labels = labels)
  var_label(variable) = var_label(x)
  return(variable)
}

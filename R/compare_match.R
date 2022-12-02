#' Check for match and unmatched variable elements, including strings
#'
#' A function to check for matched and unmatched elements between two vector and returns a list with both matched and unmatched elements.
#' E.g use match <- (compare_match(var1,var2))$Matched ,to obtain matched elements. which can also be converted to vector using as.vector.
#'
#'
#'@example compare_match()
#'
#' @export
#' @param  var1 first variable to compare
#' @param  var2 - second variable


# -----  To identify the column names in the dictionary
compare_match <- function(var1,var2){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  match = if_else(var1%in%var2,var1,as.character(NA))
  Not_match = if_else(!var1%in%var2,var1,as.character(NA))
  return(list("Matched" = na.omit(match),"Unmatched"= na.omit(Not_match)))
}






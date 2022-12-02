#' Check for match and replace the match with another string word
#'
#' Function used to check for a match in a string vector then replace the matched text with a string of interest,
#' while maintaining the unmatched elements in the variable.
#'
#'
#' @export
#' @param  variable - the variable to check the match on
#' @param  match_text - text to find a match for
#' @param replace_text - text to replace the match



#---- Create match function
replace_match <- function(variable,match_text,replace_text){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  return(if_else(is.na(str_match(variable,match_text)),variable,replace_text))
}

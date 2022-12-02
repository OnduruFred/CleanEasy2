#' Create factor levels labels variable.
#'
#' Create factor variable with labels ones supplied with the vector and the list housing both levels and labels.
#' e.g suppose you have this levels and labels and you want to map them in a vector as factor
#' list1 <- list(levels =c(1,2,3),labels =c("a","b","c")),   You simply ; var_1 <- lev_labl(variable,list1)
#'
#'
#' @export
#' @param  x variable to factor
#' @param  lst - a list with levels and labels specified
#' @param  ... other functions

#----- Develop a function ----
lev_labl <- function(x,lst,...){
  return(factor(x,
                levels = lst[[1]],  #  1 calls for first vector in the list
                labels = lst[[2]]))    #  2 calls for first vector in the list
}




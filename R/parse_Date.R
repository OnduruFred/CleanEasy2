#' Converts POSIXct format to date
#'
#' A function helps to convert Posixct date to normal dates. It also support string date.
#'
#'
#'@example parse_Date()
#'
#' @export
#' @param  x variable
#' @param  format - date format i.e.only supports (mdy,ymd,dmy), default is mdy


#  Converts POSIXct format to date
parse_Date <- function(x,format ="mdy"){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  if(!require(lubridate)){install.packages(lubridate); library(lubridate)}
  suppressWarnings({
    x = as.character(x)
    x = as.data.frame(x)
    x = x %>% separate(x,into = "x",sep = " ")
    if(format == "mdy"){
      return(mdy(x$x))
    }else if(format == "dmy"){
      return(dmy(x$x))
    } else if(format == "ymd"){
      return(ymd(x$x))
    }else
    {break}
  })
}

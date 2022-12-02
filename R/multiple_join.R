#' Join multiple data sets.
#'
#' The function help to Join multiple data set stored in a list. It perform four types of dplyr joins left_join, right_join,
#' full_join and anti_join. By default, the join type is left_join.
#'
#'
#' @export
#' @param  df_list a list of all data sets.
#' @param  type_join type of join i.e ("left_join","right_join","full_join" & "anti_join").
#' @param ...  any other function


# ------ Join multiple data set stored in a list


multiple_join <- function(df_list,type_join = "left_join",...){
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}

  new_data = df_list[[1]]  # pull the first element to start match
  df_list[[1]] = NULL

  if(type_join == "left_join"){
    for (i in 1:length(df_list)) {
      new_data = new_data %>%  left_join(df_list[[i]])
    }
  }else if(type_join == "right_join"){
    for (i in 1:length(df_list)) {
      new_data = new_data %>%  right_join(df_list[[i]])
    }
  }else if(type_join == "full_join"){
    for (i in 1:length(df_list)) {
      new_data = new_data %>%  full_join(df_list[[i]])
    }
  }else if(type_join == "anti_join"){
    for (i in 1:length(df_list)) {
      new_data = new_data %>%  anti_join(df_list[[i]])
    }
  }else{
    print("Unsupported type of match; function only work with left_join,right_join,full_join and anti_join")
  }
  return(new_data )
}

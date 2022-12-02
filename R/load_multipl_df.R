#' Load Multiple data set in R.
#'
#' The function help to load multiple data set from a specified folder within the working directory and save the results as a list.
#' Specifying the folder location works with here package. which help to obtain current working directory
#'
#' @export
#' @param  data_folder a folder where Data is stored.
#' @param  file_format file format ie (.csv, .dta, .rds, .xlsx)
#' @param ...  any other function


#----  Load Multiple data set

load_multipl_df = function(data_folder,file_format = ".rds",...){
  if(!require(here)){install.packages(here); library(here)}
  if(!require(tidyverse)){install.packages(tidyverse); library(tidyverse)}
  if(!require(haven)){install.packages(haven); library(haven)}
  if(!require(writexl)){install.packages(writexl); library(writexl)}


  ldf = list()  #  create an empty list
  lstrds = dir(path = here(data_folder),pattern = file_format)
  for (i in 1:length(lstrds)) {
    if(file_format == ".rds"){
      ldf[[i]] = readRDS(here(data_folder,lstrds[i]))
    } else if(file_format == ".dta"){
      ldf[[i]] = read_dta(here(data_folder,lstrds[i]))
    } else if(file_format == ".csv"){
      ldf[[i]] = read.csv(here(data_folder,lstrds[i]))
    } else if(file_format == ".xlsx"){
      ldf[[i]] = read_excel(here(data_folder,lstrds[i]))
    }else{
      print("Kindly make sure you write the file format starting with '.' .   NOTE:: The function only work with either .rds, .dta, .csv, .xlsx")
    }
  }

  names(ldf) = str_replace(lstrds,pattern = file_format,"")
  rm(list = ls()[!(ls()%in%c("ldf"))])
  return(ldf)
}

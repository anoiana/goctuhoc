if(!require(pacman)) install.packages("pacman");pacman::p_load(tidyverse)

newdate_func = function(update){
 if(!file.exists('date_list.rds')){saveRDS(list(), file = 'date_list.rds')}

 filename=knitr::current_input(dir = FALSE)
 mylist <- readRDS(file ='date_list.rds')

 if(update|!(filename%in%names(mylist))){
  mylist[[filename]]<- format(Sys.time(),format = c('%d-%m-%Y') )
  saveRDS(mylist, file = 'date_list.rds')}
 readRDS(file ='date_list.rds')[[filename]]
}
##########################################################################################
publish_func = function(publish){

  file_name = stringr::str_replace(knitr::current_input(dir = FALSE),"Rmd$","html")
  if(file.exists(paste0("../docs/",file_name))){
    file.copy(file_name, "../docs", overwrite  = TRUE)
  } else{
    file.copy(file_name, "../docs")
  }
}

######################################################################################
totitle = function(x){
  paste("&#128205;", stringr::str_to_title(x))
}

if(!require(pacman)) install.packages("pacman");library(pacman);p_load(
  tidyverse, magrittr, furrr,Hmisc, grid, ggplotify, knitr, kableExtra,ddpcr, yarrr
)
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
anh = list(info = list())
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#



#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 1: help_nice_form ----
anh$help_nice_form = Vectorize(function(a=NULL,b=NULL,k=NULL, digits=2, divider = " to "){
 
 list_value = list(a,b,k)%>% map_if(is.numeric,round,digits = digits)
 
 if(sum(sapply(list_value, is.null))==3) stop("at least one value must be given")
 
 values<- # transform values to characters 
  map_chr(list_value, ~format(.,digits = digits, nsmall = digits)%>% 
          stringr::str_remove_all("\\.0+$"))
 values[values=="NA"]<- NA
 values<- values[values!="NULL"]
 
 switch(length(values), # check in how many values are given  
        values[1], # if only a, return a in character form
        stringr::str_c(values[1]," (",values[2],")"), # if both a and b, return "a (b)"
        stringr::str_c(values[1], " (",values[2],divider,values[3],")") # if all three, return "a (b to c)"
 )
}, vectorize.args = c("a","b","k")) 
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$help_nice_form = function() message(
"Convert numeric to character:
\u2022 one value is given: return character number.
\u2022 two values are given: return a (b).
\u2022 three values are given: return a (b to c)")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 2: cp ----
anh$cp = purrr::partial(purrr::compose, .dir = "forward")
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$cp = function() message("compress multiples functions into one function")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 3: draw_table ----
anh$draw_table = function(df,colour = "navy" , title = NULL, notes = NULL, left.align = NULL, 
                          right.align= NULL, .cex = 0.9, .margin = 3,rowname = NULL){
 
 just_c = rep(0.5,ncol(df))
 just_c[left.align]<- 0
 just_c[right.align]<- 1
 just<- rep(just_c, each = nrow(df))
 
 color = map_chr(c(0.1,0.8,0.9),~yarrr::transparent(colour, trans.val =.))
 
 mytheme = 
  gridExtra::ttheme_default(
   # general setting
   core=list(fg_params=list(hjust=just, x=just, cex = .cex ),
             bg_params = list(fill = color[2:3] ), 
             padding = unit(c(.margin,.margin),"mm")),
   # column head setting
   colhead = list(bg_params = list(fill = color[1]),
                  fg_params=list(col = "snow", cex = .cex))
  )
 
 mytab <- gridExtra::tableGrob(df,theme = mytheme, rows = rowname)
 mytitle <- grid::textGrob(title, just = "center")
 mynotes<- grid::textGrob(notes, just = "left", hjust = 0, x = 0)
 padding <- unit(1,"line")
 mytab <- gtable::gtable_add_rows(mytab, heights = grid::grobHeight(mytitle) + padding, pos = 0)
 mytab <- gtable::gtable_add_rows(mytab, heights = grid::grobHeight(mynotes) + padding)
 r = gtable::gtable_add_grob(mytab, list(mytitle,mynotes),t = c(1, nrow(mytab)), l = c(1,2), r = ncol(mytab))
 
 grid.newpage() # clean plots window
 grid.draw(r) # draw table
 #message("TABLE: ", title)
 
 invisible(r)

} 
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$draw_table = function() message(
"Make a table as plots. We can change backgroud color and align (center is default)")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 4: get_dir ----
anh$get_dir = .%>% {str_remove(file.choose(), getwd())%>% str_remove("^/")} 
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$get_dir = function() message("return directory of the file of interest started from working directory")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 5:open.it ----
anh$open.it = function(){
 mydir = purrr::safely(file.choose)()
 if(is.null(mydir$error)){
  file.edit(mydir$result)
 }
 else message("No directory is provided")
}
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$open.it = function() message("open file dynamically")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 6: check_func ----
anh$check_func = function(df, footn =NULL){
  
 check_list = rep("\u23F3",nrow(df));
 
 rname = str_c(1:nrow(df),"\u23f5")

 function(r=NULL, okay = TRUE, col.rm=NULL, show.script = FALSE, myalign = "c", color = "navy",
          cap = NULL, posit = "center", full.width = FALSE, kable.arg = list(), styling.arg = list()){
   
  if(is.logical(okay)) check_list[r]<<- ifelse(okay,"\u2615","\u26D4")
  else  check_list[r]<<- okay
  check_list<<- check_list[1:nrow(df)]
  dat = bind_cols(df, check = check_list)

  dat%<>% mutate(id = rname, .before =1)
  
  kable.arg = append(kable.arg,
                     list(x = switch(is.null(col.rm)+1,dat[,-col.rm],dat), 
                          format = "html",
                          align = myalign,
                          caption = cap
                          ))
  kable.arg = kable.arg[unique(names(kable.arg))]
  kable.result =  purrr::lift_dl(knitr::kable)(kable.arg)
  
  styling.arg = append(styling.arg,
                       list(kable_input = kable.result, 
                            full_width = full.width, 
                            position = posit)) 
  styling.arg = styling.arg[unique(names(styling.arg))]
  
  result<-
    purrr::lift_dl(kableExtra::kable_styling)(styling.arg)%>%
      add_footnote(footn, notation = "symbol")%>%
      row_spec(row = 0, color = "white", font_size = 14, background = color)%>%
      row_spec(row = anh$cp(nrow,seq_len,~.[.%%2==0])(dat),
               background = yarrr::transparent(color, trans.val =0.8))%>%
      row_spec(row = anh$cp(nrow,seq_len,~.[.%%2!=0])(dat),
               background = yarrr::transparent(color, trans.val =0.7))%>%
      row_spec(row = seq_len(nrow(df)), color = "black")
    
  if(show.script){
   result
  } else ddpcr::quiet(result)
  }
 }
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$check_func = function() message(
"create a check list frame. argument 'okay' alllows three options:
1. okay = TRUE (default): check mark (have a cup of coffee)
2. okay = FALSE: not available
3. okay = <strings>: comments.
Also, use '<br>' instead of '\\n' to make a new line")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 7: modify check_func ----

anh$check_func_addrow<- function(my.fun.check, ...){
  addrow = list(...)

  rlang::fn_env(my.fun.check)$df%<>%
    {purrr::lift_dl(add_row, .data = .)(addrow)}
    
  rlang::fn_env(my.fun.check)$rname <- paste0(seq_len(nrow(rlang::fn_env(my.fun.check)$df)),"\u23f5")
  rlang::fn_env(my.fun.check)$check_list<- rep("\u23F3",nrow(rlang::fn_env(my.fun.check)$df))
}
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$check_func_addrow<- function() cat("add rows to check_func")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 8: get data frame of label from named vector of labels

anh$get.label = function(x,...){
  d = tibble(lab = names(x), name = x)
  anh$check_func(d,...)
}
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$get.label = function() cat("get data frame of label from named vector of label")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

anh$if.else<- function(cond, true, false, missing = NA){
  stopifnot(length(cond)==1)
  index = dplyr::if_else(cond,1,2,3)
  switch(index, true, false, missing)
}
#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#++++#+++#++++#+++#++++#
anh$info$if.else<- function() cat("The condition of this if.else must be one logical value and return any tpye of object")

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# function 8: obtain OR,RR, RD ----
anh$compare_prop<-
 function(p1 = NULL,p2 = NULL, n1 = NA, n2 = NA, ci.p = "armitage", meta = F){
  
  
  # p1 & p1 must be provided
  if(is.null(p1) | is.null(p2)) stop("p1 or p2 are not provided")
  # get n1 if not provided
  if(is.na(n1) | !as.character(n1)%>% {stringr::str_detect(.,paste0("\\d{",stringr::str_length(.),"}"))}){
   repeat{
    n1 = rstudioapi::showPrompt(title = "require sample size", message = "what is the size of sample 1?")
    check = stringr::str_detect(n1,paste0("\\d{",stringr::str_length(n1),"}"))
    if(check) break
   }
   n1 = as.numeric(n1)
  }
  # get n2 if not provided
  if(is.na(n2) | !as.character(n2)%>% {stringr::str_detect(.,paste0("\\d{",stringr::str_length(.),"}"))}){
   repeat{
    n2 = rstudioapi::showPrompt(title = "require sample size", message = "what is the size of sample 2?")
    check = stringr::str_detect(n2,paste0("\\d{",stringr::str_length(n2),"}"))
    if(check) break
   }
   n2 = as.numeric(n2)
  }
  
  
  results = list()
  
  ci.p = ifelse(meta,"standard",ci.p)
  # CI of each proportion
  if(ci.p=="armitage"){
   A = 2*p1+qnorm(0.975)^2/n1
   B = qnorm(0.975)*sqrt((qnorm(0.975)/n1)^2 + 4*p1*(1-p1)/n1)
   C = 2*(1+ qnorm(0.975)^2/n1)
   low.p1 = (A-B)/C
   high.p1 = (A+B)/C
   
   A = 2*p2+qnorm(0.975)^2/n2
   B = qnorm(0.975)*sqrt((qnorm(0.975)/n2)^2 + 4*p2*(1-p2)/n2)
   C = 2*(1+ qnorm(0.975)^2/n2)
   low.p2 = (A-B)/C
   high.p2 = (A+B)/C
  } else if(ci.p == "exact"){
   val = PropCIs::exactci(round(p1*n1),n1,0.95)
   low.p1 = val$conf.int[1]
   high.p1 = val$conf.int[2]
   
   val = PropCIs::exactci(round(p2*n2),n2,0.95)
   low.p2 = val$conf.int[1]
   high.p2 = val$conf.int[2]
  } else if(ci.p=="standard"){
   low.p1 = p1 - qnorm(0.975)*sqrt(p1*(1-p1)/n1)
   high.p1 = p1 + qnorm(0.975)*sqrt(p1*(1-p1)/n1)
   low.p2 = p2 - qnorm(0.975)*sqrt(p2*(1-p2)/n2)
   high.p2 = p2 + qnorm(0.975)*sqrt(p2*(1-p2)/n2)
  }
  results$P1 = p1
  results$conf.low.P1 = low.p1
  results$conf.high.P1 = high.p1
  results$P2 = p2
  results$conf.low.P2 = low.p2
  results$conf.high.P2 = high.p2
  
  var.p1 = p1*(1-p1)/n1 # var of prop 1
  var.p2 = p2*(1-p2)/n2 # var of prop 2
  
  #relative risk
  l.rr = log(p1/p2)
  var.l.rr = (1/p1)^2*var.p1 + (1/p2)^2*var.p2
  results$RR = exp(l.rr)
  results$`conf.low.RR` = exp(l.rr  - qnorm(0.975)*sqrt(var.l.rr))
  results$`conf.high.RR` = exp(l.rr  + qnorm(0.975)*sqrt(var.l.rr))
  results$p.val.RR = pnorm(-abs(l.rr), sd = sqrt(var.l.rr))*2
  #odd ratio
  l.or = log((p1/(1-p1))/(p2/(1-p2)))
  var.l.or = (1/(p1*(1-p1)))^2*var.p1 + (1/(p2*(1-p2)))^2*var.p2
  results$OR = exp(l.or)
  results$conf.low.OR = exp(l.or - qnorm(0.975)*sqrt(var.l.or))
  results$conf.hight.OR = exp(l.or + qnorm(0.975)*sqrt(var.l.or))
  results$p.val.OR = pnorm(-abs(l.or),sd = sqrt(var.l.or))*2
  # risk difference
  if(meta){ # use approach of meta-analysis
   rd = p1-p2
   results$RD = rd
   results$conf.low.RD = rd - qnorm(0.975)*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
   results$conf.high.RD = rd + qnorm(0.975)*sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2)
   results$p.val.RD = pnorm(rd, sd = sqrt(p1*(1-p1)/n1 + p2*(1-p2)/n2))*2
  } else{ # use approach of Armitage
   rd = p1-p2
   results$RD = rd
   results$conf.low.RD = rd - sqrt((p1- low.p1)^2 + (high.p2 - p2)^2)
   results$conf.high.RD = rd + sqrt((p2- low.p2)^2 + (high.p1 - p1)^2)
  }
  as_tibble(results)
 }

#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#
#+++#++++#++++#++++#++++#++++#++++#++++#+++#+++#++++#++++#++++#++++#++++#++++#++++#+++#++++#+++#++++#+++#++++#

# for params date 

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



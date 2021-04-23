draw_diagram = function(dat, invisible = FALSE){
  


  g = ggplot()+ xlim(0,1 )+ ylim(0,1)
  dot_layer<-geom_point(data = dat, aes(x,y), size = 30, color = "pink")
  lab_layer<- geom_text(data = dat, aes(x,y, label = lab), size = 8)
  #print(g+ dot_layer+lab_layer)
  
  myedge = NULL
  seg_layer = NULL
  my_text = NULL
  myplot<- NULL
  
  draw_plot = function() {
    myplot<<- g+ seg_layer + dot_layer+ lab_layer+ my_text;
    if(!invisible) print(myplot)
    }
  
  l<-
  list(
    
    edge = function(edge, lt = NULL){
      
      direct<-
      purrr::map(edge, ~str_split(., pattern = "")[[1]])%>%
        purrr::reduce(rbind)%>%
        purrr::compose(as.data.frame,as_tibble, .dir = "forward")()
      
      myedge<<-
      tibble(
        x.from = dat$x[match(direct$V1,dat$lab)],
        y.from = dat$y[match(direct$V1,dat$lab)],
        x.to = dat$x[match(direct$V2,dat$lab)],
        y.to = dat$y[match(direct$V2,dat$lab)],
        lt = switch(is.null(lt)+1,lt,rep("dashed",length(edge)))
      )
      
      seg_layer <<- 
        geom_segment(data = myedge, aes(x = x.from, y = y.from, xend = x.to, yend = y.to,linetype = lt),
                     show.legend = F, color = "gray60")
      draw_plot()
    },
    
    mytext = function(mytext){
      my_text<<- geom_text(data = mytext, aes(x = y,y = y, label = text))
      draw_plot()
      },
    
    finalize = function(arrow = FALSE){
      
      if(arrow){
        myedge<- mutate(myedge, x.to = (x.from+x.to)/2, y.to = (y.to+y.from)/2,
                        x.from = (x.from+x.to)/2, y.from = (y.to+y.from)/2)
         myplot+ geom_segment(data = myedge, aes(x = x.from, y = y.from, xend = x.to, yend = y.to,linetype = lt),
                              show.legend = F, color = "gray60", arrow = arrow(length = unit(0.5, "cm")))+
           theme_void()
      } else myplot+ theme_void()
      
      
      }
  )
  draw_plot(); return(l)
  
}


# dat = tibble(x = c(0.1,0.3,0.5), y = c(0.1,0.5,0.1), lab = c("A","B","C"))
# edge = c("AB","BC","CA")
# #lt =  c("dashed","dashed","dotted")
# mytext = tibble(text = c("first line","second line","third line"), x = c(0,0.25,0.5), y = c(0.25,0.5,0.75))
# 
# l = draw_diagram(dat)
# l$edge(edge)
# l$mytext(mytext)
# l$finalize(T)

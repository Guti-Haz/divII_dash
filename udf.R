genRandom=function(){
  runif(1,1e7,1e8) %>% 
    round(0) %>% 
    as.character
}

makeButton=function(type,ref,Icon){
  js=paste0('Shiny.setInputValue(\"',type,'_button\",this.id,{priority:"event"})')
  paste0(type,"_",ref) %>% 
    actionButton(.,label="",icon(Icon),style=css.button,onclick=js) %>% 
    as.character
}

adjTypetoDB=function(dt){
  data=dt[Type_Natural==T,Type_Natural:=1] %>% 
    .[Type_Legal==T,Type_Legal:=1] %>% 
    .[Type_Natural==F|is.null(Type_Natural)|is.na(Type_Natural),Type_Natural:=0] %>% 
    .[Type_Legal==F|is.null(Type_Legal)|is.na(Type_Legal),Type_Legal:=0]
  return(dt)
}

adjDBtoType=function(dt){
  .data=dt[,Type_Natural:=as.logical(Type_Natural)] %>% 
    .[,Type_Legal:=as.logical(Type_Legal)]
  return(data)
}
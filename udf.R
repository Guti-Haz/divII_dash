genRandom=function(){
  runif(1,1e7,1e8) %>% 
    round(0) %>% 
    as.character
}
makeButton=function(task,type,ref){
  js=paste0('Shiny.setInputValue(\"',task,"_",type,'_button\",this.id,{priority:"event"})')
  paste0(type,"_",ref) %>% 
    actionButton(.,label=type,style=css.button,onclick=js) %>% 
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
  data=dt[,Type_Natural:=as.logical(Type_Natural)] %>% 
    .[,Type_Legal:=as.logical(Type_Legal)]
  return(data)
}
adjType=function(dt){
  data=melt(dt,measure.vars=c("Type_Natural","Type_Legal"),variable.name="Type") %>% 
    .[value==T] %>% 
    .[,!"value"] %>% 
    .[,Type:=str_replace(Type,"^Type_","")] %>% 
    setcolorder(c(1,2,3,7,4,5,6))
  return(data)
}
errorUI=function(vec){
  {if(length(vec[vec==F])>0) vec[vec==F]} %>% 
    names %>% 
    paste0(collapse=", ") %>%
    paste0("  Incomplete fields: <font color='red'> <b>",.,"</b></font>") %>% 
    HTML
}
ingest=function(pool,tbl,data){
  sprintf('INSERT INTO %s (%s) VALUES (%s)',
          tbl,
          paste0(names(data),collapse=","),
          paste0(paste0("'",data,"'"),collapse=",")) %>% 
    dbExecute(pool,.)
}
update=function(pool,tbl,data,row){
  sprintf("UPDATE %s SET %s WHERE RefNum_external='%s'",
          tbl,
          paste0(names(data),"='",data,"'",collapse=","),
          row) %>% 
    dbExecute(pool,.)
}
emptyToNA=function(dt){
  for(j in names(dt)){
    set(dt,j=j,value=ifelse(dt[[j]]=="",NA,dt[[j]]))
  }
  return(dt)
}
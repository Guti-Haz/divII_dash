genRandom=function(){
  runif(1,1e7,1e8) %>% 
    round(0) %>% 
    as.character
}
makeButton=function(task,type,ref){
  js=paste0('Shiny.setInputValue(\"',task,"_",type,'_button\",this.id,{priority:"event"})')
  paste0(task,"_",type,"_",ref) %>% 
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
    paste0("  Status: <font color='red'> <b> Incomplete fields (",.,")</b></font>") %>% 
    HTML
}
successUI=function(){
  paste0("  Status: <font color='blue'> <b> Submitted! </b></font>") %>% 
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
in.req_fx=function(pool,validity,daysElapsed){
  in_req=setDT(dbReadTable(pool,"in_req")) %>% 
    .[dmy(Date_received)>=dmy(prime_start)]
  out_resp=setDT(dbReadTable(pool,"out_resp")) %>% 
    .[!is.na(RefNum_external)]
  valid=setDT(dbReadTable(pool,"valid")) %>% 
    .[!(!is.na(Date_ask)&!is.na(Date_reply))]
  data=merge(in_req,out_resp,by="RefNum_external",all.x=T) %>% 
    merge(valid,by="RefNum_external",all.x=T) %>% 
    setDT %>% 
    emptyToNA %>% 
    .[is.na(Date_responsed),Status:=paste0("Pending - ",as.integer(today()-dmy(Date_received))," days elapsed")] %>% 
    .[!is.na(Date_ask)&is.na(Date_reply),Status:="Invalidated"] %>% 
    .[is.na(Analyst),a:=sapply(RefNum_external,function(x){makeButton("in.req","assign",x)})] %>%
    .[Nature=="Egmont"&is.na(Date_responsed),b:=sapply(RefNum_external,function(x){makeButton("in.req","invalidate",x)})] %>%
    .[Nature=="Egmont"&!is.na(Date_ask)&is.na(Date_responsed),b:=sapply(RefNum_external,function(x){makeButton("in.req","revalidate",x)})] %>%
    .[Status!="Invalidated",c:=sapply(RefNum_external,function(x){makeButton("in.req","response",x)})] %>%
    .[!is.na(Status)] %>% 
    {if (validity) .[Status!="Invalidated"] else .[Status=="Invalidated"]} %>% 
    .[as.integer(today()-dmy(Date_received))>=daysElapsed] %>% 
    .[,Date_received:=dmy(Date_received)] %>% 
    .[order(-Date_received)] %>% 
    .[,.(
      Nature,
      Partner,
      RefNum_external,
      Date_received,
      Date_responsed,
      Analyst,
      Status,
      a,
      b,
      c
    )]
  return(data)
}
out.req_fx=function(pool){
  out_req=setDT(dbReadTable(pool,"out_req")) %>% 
    .[dmy(Date_ask)>=dmy(prime_start)]
  in_resp=setDT(dbReadTable(pool,"in_resp"))
  data=setDT(merge(out_req,in_resp,by="RefNum_internal",all.x=T)) %>% 
    emptyToNA %>% 
    .[is.na(Date_reply),Status:=paste0("Pending - ",as.integer(today()-dmy(Date_ask))," days elapsed")] %>%
    .[is.na(Date_reply),a:=sapply(RefNum_internal,function(x){makeButton("out.req","response",x)})] %>%
    .[!is.na(Status)] %>%
    .[,Date_ask:=dmy(Date_ask)] %>%
    .[order(-Date_ask)] %>%
    .[,.(Partner,
         RefNum_internal,
         Date_ask,
         RefNum_external,
         Date_reply,
         Analyst,
         Status,
         a)]
  return(data)
}
in.spon_fx=function(pool){
  data=setDT(dbReadTable(pool,"in_spon")) %>% 
    .[dmy(Date)>=dmy(prime_start)] %>% 
    emptyToNA %>% 
    .[is.na(Analyst),a:=sapply(RefNum_external,function(x){makeButton("in.spon","assign",x)})] %>%
    .[is.na(Analyst)] %>% 
    .[,Date:=dmy(Date)] %>% 
    .[order(-Date)] %>% 
    .[,.(Partner,
         RefNum_external,
         Date,
         Analyst,
         a)]
  return(data)
}
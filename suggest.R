pool=dbPool(drv=RSQLite::SQLite(),dbname="data.sqlite")

in_req=setDT(dbReadTable(pool,"in_req")) %>% 
  .[dmy(Date_received)>=dmy(prime_start)]
out_resp=setDT(dbReadTable(pool,"out_resp")) %>% 
  .[!is.na(RefNum_external)]
valid=setDT(dbReadTable(pool,"valid")) %>% 
  .[!(!is.na(Date_ask)&!is.na(Date_reply))]

data=merge(in_req,out_resp,by="RefNum_external",all.x=T) %>% 
  merge(valid,by="RefNum_external",all.x=T) %>% 
  setDT %>% 
  .[is.na(Date_responsed),Status:="Pending"] %>% 
  .[Nature=="Egmont"&!is.na(Date_ask)&is.na(Date_reply),Status:="Invalidated"] %>% 
  .[Status=="Invalidated"&!is.na(Date_reply),Status:="Revalidated"] %>% 
  .[Status%in%c("Pending","Revalidated")] %>%
  .[!is.na(Analyst)] %>%
  .[,daysElapsed:=as.numeric(today()-dmy(Date_received))] %>% 
  .[,.(Nature,daysElapsed,Analyst)] %>% 
  .[,.(num=.N,mean_daysElapsed=round(sum(daysElapsed)/.N,0)),by=c("Analyst","Nature")] %>% 
  .[order(Nature,num,mean_daysElapsed)]

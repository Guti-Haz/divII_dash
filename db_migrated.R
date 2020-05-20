setwd(dirname(rstudioapi::getSourceEditorContext()$path))
source("loadLibs.R")
# corr ----
## initiate ----
db="corr.sqlite"
if(file.exists(db)){file.remove(db)}
conn=dbConnect(SQLite(),db)
### str ----
tblName="str_tbl"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Nature="TEXT",
  Type="TEXT",
  RefNum_external="TEXT",
  RefNum_internal="TEXT",
  STR="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### subject ----
tblName="subject"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Nature="TEXT",
  Type="TEXT",
  RefNum_external="TEXT",
  RefNum_internal="TEXT",
  Type_Natural="INTEGER",
  Type_Legal="INTEGER",
  Name="TEXT",
  ID="TEXT",
  Account="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### in_req ----
tblName="in_req"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Nature="TEXT",
  Partner="TEXT",
  RefNum_external="TEXT",
  Date_received="TEXT",
  Analyst="TEXT",
  Offence_received="TEXT",
  OffenceDesc_received="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### valid ----
tblName="valid"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(RefNum_external="TEXT",
  Date_ask="TEXT",
  Reason="TEXT",
  Detail_inquiry="TEXT",
  Date_reply="TEXT",
  Detail_response="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### out_resp ----
tblName="out_resp"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Supervisor="TEXT",
  Workfile="TEXT",
  Complexity="TEXT",
  AnyIntel="TEXT",
  OtherInfo="TEXT",
  Offence_responsed="TEXT",
  OffenceDesc_responsed="TEXT",
  Law="TEXT",
  Prov="TEXT",
  RefNum_external="TEXT",
  RefNum_internal="TEXT",
  Date_responsed="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### out_req ----
tblName="out_req"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Partner="TEXT",
  Analyst="TEXT",
  Supervisor="TEXT",
  OnBehalf="TEXT",
  LeaOfficer="TEXT",
  RefNum_internal="TEXT",
  Date_ask="TEXT",
  Workfile="TEXT",
  Complexity="TEXT",
  Offence_asked="TEXT",
  OffenceDesc_asked="TEXT",
  Law="TEXT",
  Prov="TEXT",
  OtherInfo="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### in_resp ----
tblName="in_resp"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(RefNum_external="TEXT",
  RefNum_internal="TEXT",
  Date_reply="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### out_spon ----
tblName="out_spon"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Nature="TEXT",
  Partner="TEXT",
  Analyst="TEXT",
  Supervisor="TEXT",
  RefNum="TEXT",
  Date="TEXT",
  Workfile="TEXT",
  Complexity="TEXT",
  Main="TEXT",
  Offence="TEXT",
  OffenceDesc="TEXT",
  Law="TEXT",
  Prov="TEXT",
  OtherInfo="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### in_spon ----
tblName="in_spon"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Partner="TEXT",
  Offence="TEXT",
  OffenceDesc="TEXT",
  RefNum_external="TEXT",
  Date="TEXT",
  Analyst="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
## migrate ----
conn2=dbConnect(SQLite(),"data/AdminT1.db")
### str ----
domestic=setDT(dbReadTable(conn2,"out_domcor")) %>% 
  .[Type%in%c("Respond","Sharing")] %>%
  .[Type=="Respond",Type:="out_resp"] %>% 
  .[Type=="Sharing",Type:="out_spon"] %>% 
  .[!STR_list%in%c("","0")] %>% 
  .[,.(Type,Ref_LEA,Ref_FIED,STR_list)] %>% 
  unique %>% 
  lapply(seq(nrow(.)),function(x,dt){
    data.table(Type=dt$Type[x],
               RefNum_external=dt$Ref_LEA[x],
               RefNum_internal=dt$Ref_FIED[x],
               STR=unlist(str_split(dt$STR_list[x],"\n")))
  },dt=.) %>% 
  rbindlist %>% 
  .[STR!=""] %>% 
  .[,Nature:="Domestic"]
foreign=setDT(dbReadTable(conn2,"out_forcor")) %>% 
  .[Type%in%c("Respond","Sharing")] %>%
  .[Type=="Respond",Type:="out_resp"] %>% 
  .[Type=="Sharing",Type:="out_spon"] %>% 
  .[!STR_list%in%c("","0")] %>% 
  .[,.(Type,Ref_FFIU,Ref_FIED,STR_list)] %>% 
  unique %>% 
  lapply(seq(nrow(.)),function(x,dt){
    data.table(Type=dt$Type[x],
               RefNum_external=dt$Ref_FFIU[x],
               RefNum_internal=dt$Ref_FIED[x],
               STR=unlist(str_split(dt$STR_list[x],"\n")))
  },dt=.) %>% 
  rbindlist %>% 
  .[STR!=""] %>% 
  .[,Nature:="Foreign"]
mla=setDT(dbReadTable(conn2,"out_mla")) %>% 
  .[!STR_list%in%c("","0")] %>% 
  .[,.(Ref_MLA,Ref_FIED,STR_list)] %>% 
  unique %>% 
  lapply(seq(nrow(.)),function(x,dt){
    data.table(RefNum_external=dt$Ref_MLA[x],
               RefNum_internal=dt$Ref_FIED[x],
               STR=unlist(str_split(dt$STR_list[x],"\n")))
  },dt=.) %>% 
  rbindlist %>% 
  .[STR!=""] %>% 
  .[,Nature:="MLA"] %>% 
  .[,Type:="out_resp"]
dt_final=list(domestic,foreign,mla) %>% 
  rbindlist(use.names=T)
dbAppendTable(conn,"str_tbl",dt_final)
### subject ----
domestic=setDT(dbReadTable(conn2,"in_domcor")) %>% 
  .[Type=="Request"] %>%
  .[Entities!=""] %>% 
  .[,.(Ref_LEA,Entities)] %>% 
  lapply(seq(nrow(.)),function(x,dt){
    data.table(RefNum_external=dt$Ref_LEA[x],
               Name=str_to_upper(str_trim(unlist(str_split(dt$Entities[x],"\n|,")))))
  },dt=.) %>% 
  rbindlist %>% 
  .[!Name%in%c("","NA")] %>% 
  .[,Name:=str_replace(Name," & \\d{1,3} OTHERS","")] %>% 
  .[,ID:=str_extract(Name,"\\d{6}-\\d{2}-\\d{4}|\\d{3,}[:upper:]|Passport.*")] %>% 
  .[,Nature:="Domestic"]
foreign=setDT(dbReadTable(conn2,"in_forcor")) %>% 
  .[Type=="Request"] %>%
  .[Entities!=""] %>% 
  .[,.(Ref_FFIU,Entities)] %>% 
  lapply(seq(nrow(.)),function(x,dt){
    data.table(RefNum_external=dt$Ref_FFIU[x],
               Name=str_to_upper(str_trim(unlist(str_split(dt$Entities[x],"\n|,")))))
  },dt=.) %>% 
  rbindlist %>% 
  .[!Name%in%c("","NA")] %>% 
  .[,Name:=str_replace(Name," & \\d{1,3} OTHERS","")] %>% 
  .[,ID:=str_extract(Name,"\\d{6}-\\d{2}-\\d{4}|\\d{3,}[:upper:]|Passport.*")] %>% 
  .[,Nature:="Foreign"]
mla=setDT(dbReadTable(conn2,"in_mla")) %>% 
  .[Entities!=""] %>% 
  .[,.(Ref_MLA,Entities)] %>% 
  lapply(seq(nrow(.)),function(x,dt){
    data.table(RefNum_external=dt$Ref_MLA[x],
               Name=str_to_upper(str_trim(unlist(str_split(dt$Entities[x],"\n|,")))))
  },dt=.) %>% 
  rbindlist %>% 
  .[!Name%in%c("","NA")] %>% 
  .[,Name:=str_replace(Name," & \\d{1,3} OTHERS","")] %>% 
  .[,ID:=str_extract(Name,"\\d{6}-\\d{2}-\\d{4}|\\d{3,}[:upper:]|Passport.*")] %>% 
  .[,Nature:="MLA"]
dt_final=list(domestic,foreign,mla) %>% 
  rbindlist
dbAppendTable(conn,"subject",dt_final)
### in_req ----
a=fread("data/raw/forcor_new.csv",check.names=T)%>%
  .[Incoming.Outgoing%chin%c("Incoming","incoming")&Basis.of.Disclosure%chin%c("Request","request")]%>%
  .[,.(Country=Country.of...Foreign.FIU,
       Offence,
       Offence_additional=Offence..General.,
       Entities,
       Date_In=Date.Received.Sent,
       Ref_FFIU=Their.Ref.No.,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=dbReadTable(conn2,"in_forcor")%>%setDT(.)%>%
  .[year(ymd(Date_In_In))>=2019&Type=="Request"&Submit_DateTime!=""]%>%
  merge(.,
        dbReadTable(conn2,"update_analyst")%>%
          setDT(.)%>%
          .[,.(Ref_FFIU,Analyst)],by="Ref_FFIU",all.x=T,sort=F)%>%
  .[,.(Country,
       Offence,
       Offence_additional,
       Entities,
       IndividualReq,
       NonIndividualReq,
       Date_In=Date_In_In,
       Analyst,
       Ref_FFIU)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
request_from_foreign=rbindlist(list(a,b),use.names=T,fill=T)%>%
  .[,.(Nature="Egmont",
       Partner=Country,
       Offence_received=Offence,
       OffenceDesc_received=Offence_additional,
       Date_received=Date_In,
       Analyst,
       RefNum_external=Ref_FFIU)]
rm(a,b)
request_from_domestic=dbReadTable(conn2,"in_domcor")%>%setDT(.)%>%
  merge(.,dbReadTable(conn2,"update_analyst")%>%setDT(.)%>%.[,.(Ref_LEA,Analyst)],by="Ref_LEA",all.x=T,sort=F)%>%
  .[,.(Nature="Domestic",
       Partner=Agency,
       Offence_received=Offence,
       OffenceDesc_received=Offence_additional,
       Date_received=Date_In_In,
       Analyst,
       RefNum_external=Ref_LEA)]
request_from_MLA=dbReadTable(conn2,"in_mla")%>%setDT(.)%>%
  merge(.,dbReadTable(conn2,"update_analyst")%>%setDT(.)%>%.[,.(Ref_MLA,Analyst)],by="Ref_MLA",all.x=T,sort=F)%>%
  .[,.(Nature="MLA",
       Partner=Country,
       Offence_received=Offence,
       OffenceDesc_received=Offence_additional,
       Date_received=Date_In_In,
       Analyst,
       RefNum_external=Ref_MLA)]
dt_final=list(request_from_domestic,request_from_foreign,request_from_MLA) %>% 
  rbindlist %>% 
  .[,Date_received:=format(parse_date_time(Date_received,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]=="",NA,.[[j]]));.}
dbAppendTable(conn,"in_req",dt_final)
### valid ----
vout=setDT(dbReadTable(conn2,"update_inquire_out")) %>% 
  .[Ref_FFIU!=""]
vin=setDT(dbReadTable(conn2,"update_inquire_in"))
dt_final=merge(vout,vin,by="Ref_FFIU",all.x=T) %>% 
  .[,.(RefNum_external=Ref_FFIU,
    Date_ask=Date_Inquiry_Out,
    Reason,
    Detail_inquiry=Details.x,
    Date_reply=Date_Inquiry_In,
    Detail_response=Details.y)]
dbAppendTable(conn,"valid",dt_final)
### out_resp ----
a=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))<=2016&Type=="Request"&Location=="Foreign"&Processor=="Analyst"]%>%
  .[,.(Country,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       Disclosure_Type=DisclosureType,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=setDT(read_excel("data/raw/fINTEL 2017-2018 AUDIT.xlsx",.name_repair="universal"))%>%
  .[Type=="Request"&Location=="Foreign"&Processor=="Analyst"]%>%
  .[,.(Country,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
c=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))>=2019&Type=="Request"&Location=="Foreign"&Processor=="Analyst"]%>%
  .[,.(Country,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       Disclosure_Type=DisclosureType,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
d=dbReadTable(conn2,"out_forcor")%>%setDT(.)%>%
  .[year(ymd(Date_Out_Out))>=2019&Type=="Respond"&Submit_DateTime!=""]%>%
  .[,!"Analyst"]%>%
  merge(.,dbReadTable(conn2,"update_analyst")%>%setDT(.)%>%.[,.(Ref_FFIU,Analyst)],by="Ref_FFIU",all.x=T,sort=F)%>%
  .[,!c("Country","Offence","Offence_additional")]%>%
  merge(.,dbReadTable(conn2,"in_forcor")%>%setDT(.)%>%.[Type=="Request"&Ref_FFIU!="",.(Country,Offence,Offence_additional,Ref_FFIU)],by="Ref_FFIU",all.x=T,sort=F)%>%
  .[,.(Country,
       Offence,
       Offence_additional,
       IndividualDisc,
       NonIndividualDisc,
       Nothingness,
       Disclosure_Type,
       STR_list,
       NoCTR=No_CTR,
       Others,
       Main,
       Date_Out=Date_Out_Out,
       Ref_FIED,
       Workfile,
       Analyst,
       Ref_FFIU)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
respond_to_foreign=rbindlist(list(a,b,c,d),use.names=T,fill=T)%>%
  .[,.(Law=Legislation,
       Prov=Provision,
       Offence_responsed=Offence,
       OffenceDesc_responsed=Offence_additional,
       AnyIntel=Nothingness,
       Complexity=Disclosure_Type,
       OtherInfo=Others,
       Date_responsed=Date_Out,
       Workfile,
       RefNum_internal=Ref_FIED,
       RefNum_external=Ref_FFIU)]
rm(a,b,c,d)
a=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))<=2016&Type=="Request"&Location=="Domestic"&Processor=="Analyst"]%>%
  .[,.(Agency=AgencyOrFFIU,
       Offence,
       Legislation,
       Provision,
       IndividualReq,
       NonIndividualReq,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       Disclosure_Type=DisclosureType,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=setDT(read_excel("data/raw/fINTEL 2017-2018 AUDIT.xlsx",.name_repair="universal"))%>%
  .[Type=="Request"&Location=="Domestic"&Processor=="Analyst"]%>%
  .[,.(Agency=AgencyOrFFIU,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
c=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))>=2019&Type=="Request"&Location=="Domestic"&Processor=="Analyst"]%>%
  .[,.(Agency=AgencyOrFFIU,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       Disclosure_Type=DisclosureType,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
d=dbReadTable(conn2,"out_domcor")%>%setDT(.)%>%
  .[year(ymd(Date_Out_Out))>=2019&Type=="Respond"&Submit_DateTime!=""]%>%
  .[,!"Analyst"]%>%
  merge(.,dbReadTable(conn2,"update_analyst")%>%setDT(.)%>%.[,.(Ref_LEA,Analyst)],by="Ref_LEA",all.x=T,sort=F)%>%
  .[,!c("Agency","Offence")]%>%
  merge(.,dbReadTable(conn2,"in_domcor")%>%setDT(.)%>%.[Type=="Request"&Ref_LEA!="",.(Agency,Offence,Ref_LEA)],by="Ref_LEA",all.x=T,sort=F)%>%
  .[,.(Agency,
       Offence,
       IndividualDisc,
       NonIndividualDisc,
       Nothingness,
       Disclosure_Type,
       STR_list,
       NoCTR=No_CTR,
       Others,
       Main,
       Date_Out=Date_Out_Out,
       Ref_FIED,
       Workfile,
       Analyst,
       Ref_LEA)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
respond_to_domestic=rbindlist(list(a,b,c,d),use.names=T,fill=T)%>%
  .[,.(Offence_responsed=Offence,
       Law=Legislation,
       Prov=Provision,
       AnyIntel=Nothingness,
       Complexity=Disclosure_Type,
       OtherInfo=Others,
       Date_responsed=Date_Out, 
       Workfile,
       RefNum_internal=Ref_FIED, 
       RefNum_external=Ref_LEA)] 
rm(a,b,c,d)
respond_to_MLA=dbReadTable(conn2,"out_mla")%>%setDT(.)%>%
  merge(.,dbReadTable(conn2,"in_mla")%>%setDT(.)%>%.[,.(Country,Offence,Offence_additional,Ref_MLA)],by="Ref_MLA",all.x=T,sort=F)%>%
  merge(.,dbReadTable(conn2,"update_analyst")%>%setDT(.)%>%.[,.(Ref_MLA,Analyst)],by="Ref_MLA",all.x=T,sort=F)%>%
  .[,.(Offence_responsed=Offence,
       OffenceDesc_responsed=Offence_additional,
       Law=Legislation, 
       Prov=Provision, 
       Complexity=Disclosure_Type,
       AnyIntel=Nothingness, 
       OtherInfo=Others,
       Date_responsed=Date_Out_Out, 
       RefNum_internal=Ref_FIED, 
       Workfile,
       RefNum_external=Ref_MLA)] 
dt_final=list(respond_to_domestic,respond_to_foreign,respond_to_MLA) %>% 
  rbindlist(fill=T) %>% 
  .[AnyIntel=="No",AnyIntel:=T] %>% 
  .[AnyIntel=="Yes",AnyIntel:=F] %>% 
  .[,Date_responsed:=format(parse_date_time(Date_responsed,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]%in%c("","NULL"),NA,.[[j]]));.}
dbAppendTable(conn,"out_resp",dt_final)
### out_req ----
a=fread("data/raw/forcor_new.csv",check.names=T)%>%
  .[Incoming.Outgoing=="Outgoing"&Basis.of.Disclosure%chin%c("Request","request")]%>%
  .[,.(Country=Country.of...Foreign.FIU,
       Offence,
       Offence_additional=Offence..General.,
       Entities,
       Date_Out=Date.Received.Sent,
       Analyst,
       On_Behalf_Of=Outgoing.Request.on.behalf.of,
       Ref_FIED=Our.Ref.No.)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=dbReadTable(conn2,"out_forcor")%>%setDT(.)%>%
  .[Type=="Request"&Submit_DateTime!=""]%>%
  .[,.(Country,
       Offence,
       Offence_additional,
       Legislation,
       Provision,
       IndividualDisc,
       NonIndividualDisc,
       Date_Out=Date_Out_Out,
       Workfile,
       Analyst,
       On_Behalf_Of,
       LEA_Officer,
       Ref_FIED)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
dt_final=rbindlist(list(a,b),use.names=T,fill=T)%>%
  .[,.(Partner=Country,
       Law=Legislation,
       Prov=Provision,
       Offence_asked=Offence,
       OffenceDesc_asked=Offence_additional,
       Date_ask=Date_Out,
       Workfile,
       Analyst,
       OnBehalf=On_Behalf_Of,
       LeaOfficer=LEA_Officer,
       RefNum_internal=Ref_FIED)]%>% 
  .[,Date_ask:=format(parse_date_time(Date_ask,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]%in%c("","NULL"),NA,.[[j]]));.}
rm(a,b)
dbAppendTable(conn,"out_req",dt_final)
### in_resp ----
dt_final=setDT(dbReadTable(conn2,"update_respond_outRFI")) %>% 
  .[,.(RefNum_internal=Ref_FIED,
       Date_reply=Date_Out_In)] %>% 
  .[,Date_reply:=format(parse_date_time(Date_reply,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]%in%c("","NULL"),NA,.[[j]]));.}
dbAppendTable(conn,"in_resp",dt_final)
### out_spon ----
a=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))<=2016&Type=="Spontaneous"&Location=="Foreign"&Processor=="Analyst"]%>%
  .[,.(Country,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=setDT(read_excel("data/raw/fINTEL 2017-2018 AUDIT.xlsx",.name_repair="universal"))%>%
  .[Type=="Spontaneous"&Location=="Foreign"&Processor=="Analyst"]%>%
  .[,.(Country,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
c=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))>=2019&Type=="Spontaneous"&Location=="Foreign"&Processor=="Analyst"]%>%
  .[,.(Country,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
d=dbReadTable(conn2,"out_forcor")%>%setDT(.)%>%
  .[year(ymd(Date_Out_Out))>=2019&Type=="Sharing"&Submit_DateTime!=""]%>%
  .[,.(Country,
       Offence,
       Offence_additional,
       IndividualDisc,
       NonIndividualDisc,
       STR_list,
       NoCTR=No_CTR,
       Others,
       Main,
       Date_Out=Date_Out_Out,
       Ref_FIED,
       Workfile,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
sharing_to_foreign=rbindlist(list(a,b,c,d),use.names=T,fill=T)%>%
  .[,.(Nature="Egmont",
       Partner=Country,
       Law=Legislation,
       Prov=Provision,
       Offence,
       OffenceDesc=Offence_additional,
       OtherInfo=Others,
       Date=Date_Out,
       Workfile,
       RefNum=Ref_FIED,
       Main,
       Analyst)]
rm(a,b,c,d)
a=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))<=2016&Type=="Spontaneous"&Location=="Domestic"&Processor=="Analyst"]%>%
  .[,.(Agency=AgencyOrFFIU,
       Offence,
       Legislation,
       Provision,
       IndividualReq,
       NonIndividualReq,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=setDT(read_excel("data/raw/fINTEL 2017-2018 AUDIT.xlsx",.name_repair="universal"))%>%
  .[Type=="Spontaneous"&Location=="Domestic"&Processor=="Analyst"]%>%
  .[,.(Agency=AgencyOrFFIU,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
c=fread("data/raw/FINTEL responses/fintel.csv",check.names=T)%>%
  .[year(dmy(DisclosedDate))>=2019&Type=="Spontaneous"&Location=="Domestic"&Processor=="Analyst"]%>%
  .[,.(Agency=AgencyOrFFIU,
       Legislation,
       Provision,
       Offence,
       IndividualDisc=Individual,
       NonIndividualDisc=NonIndividual,
       NoSTR,
       NoCTR,
       Others,
       Date_Out=DisclosedDate,
       Workfile,
       Ref_FIED=RefNo,
       Main,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
d=dbReadTable(conn2,"out_domcor")%>%setDT(.)%>%
  .[year(ymd(Date_Out_Out))>=2019&Type=="Sharing"&Submit_DateTime!=""]%>%
  .[,.(Agency,
       Offence,
       IndividualDisc,
       NonIndividualDisc,
       Nothingness,
       STR_list,
       NoCTR=No_CTR,
       Others,
       Main,
       Date_Out=Date_Out_Out,
       Ref_FIED,
       Workfile,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
sharing_to_domestic=rbindlist(list(a,b,c,d),use.names=T,fill=T)%>%
  .[,.(Nature="Domestic",
       Partner=Agency,
       Offence,
       Law=Legislation,
       Prov=Provision,
       OtherInfo=Others,
       Date=Date_Out,
       Workfile,
       RefNum=Ref_FIED,
       Main,
       Analyst)]
rm(a,b,c,d)
dt_final=list(sharing_to_foreign,sharing_to_domestic) %>%
  rbindlist(fill=T) %>% 
  .[,Date:=format(parse_date_time(Date,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]%in%c("","NULL"),NA,.[[j]]));.}
dbAppendTable(conn,"out_spon",dt_final)
### in_spon ----
a=fread("data/raw/forcor_new.csv",check.names=T)%>%
  .[Incoming.Outgoing%chin%c("Incoming","incoming")&Basis.of.Disclosure=="Spontaneous"]%>%
  .[,.(Country=Country.of...Foreign.FIU,
       Offence,
       Offence_additional=Offence..General.,
       Entities,
       Date_In=Date.Received.Sent,
       Ref_FFIU=Their.Ref.No.,
       Analyst)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
b=dbReadTable(conn2,"in_forcor")%>%setDT(.)%>%
  .[year(ymd(Date_In_In))>=2019&Type=="Sharing"&Submit_DateTime!=""]%>%
  merge(.,dbReadTable(conn2,"update_analyst")%>%setDT(.)%>%.[,.(Ref_FFIU,Analyst)],by="Ref_FFIU",all.x=T,sort=F)%>%
  .[,.(Country,
       Offence,
       Offence_additional,
       Date_In=Date_In_In,
       Analyst,
       Ref_FFIU)]%>%
  .[,(colnames(.)):=lapply(.SD,as.character),.SDcols=colnames(.)]
dt_final=rbindlist(list(a,b),use.names=T,fill=T)%>%
  .[,.(Partner=Country,
       Offence,
       OffenceDesc=Offence_additional,
       Date=Date_In,
       Analyst,
       RefNum_external=Ref_FFIU)]%>% 
  .[,Date:=format(parse_date_time(Date,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]%in%c("","NULL"),NA,.[[j]]));.}
dbAppendTable(conn,"in_spon",dt_final)
dbDisconnect(conn)
dbDisconnect(conn2)
# cv ----
## initiate ----
db="cv.sqlite"
if(file.exists(db)){file.remove(db)}
conn=dbConnect(SQLite(),db)
### in.req ----
tblName="in.req"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Partner="TEXT",
  Date_received="TEXT",
  RefNum_external="TEXT",
  Analyst="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### subject ----
tblName="subject"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(RefNum_external="TEXT",
  Title="TEXT",
  Name="TEXT",
  ID="TEXT",
  Company="TEXT",
  Designation="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
### out.resp ----
tblName="out.resp"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Decision="TEXT",
  Remarks="TEXT",
  RefNum_internal="TEXT",
  Date.responded="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
## migrate ----
dt=read.xlsx("data/raw/CV Registry FIED.xlsx",detectDates=T,check.names=T) %>% 
  setDT %>% 
  .[,Date:=parse_date_time(Date,c("d/m/Y","Y-m-d"))] %>% 
  .[,Date:=format(parse_date_time(Date,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  .[,Date.submitted:=parse_date_time(Date.submitted,c("d/m/Y","Y-m-d"))] %>% 
  .[,Date.submitted:=format(parse_date_time(Date.submitted,c("Y-m-d","d/m/Y")),"%d-%m-%Y")] %>% 
  .[,Date:=na.locf(Date)] %>% 
  .[!str_detect(CV.No,"\\d"),CV.No:=NA] %>% 
  merge(unique(.[!is.na(CV.No),.(CV.No,Date,Department)]),by=c("Date","Department"),all.x=T,sort=F) %>% 
  .[is.na(CV.No.x)&!is.na(CV.No.y),CV.No.x:=CV.No.y] %>% 
  setnames("CV.No.x","CV.No") %>% 
  .[,CV.No.y:=NULL] %>% 
  .[is.na(CV.No),CV.No:=.GRP,by=.(Date,Department,Internal.Running.No)] %>% 
  .[,NNUA:=str_to_upper(NNUA)] %>% 
  .[,IC.No.:=str_trim(str_replace_all(IC.No.,"[:punct:]|\\s",""))] %>% 
  unique %>% 
  {for(j in names(.)) set(.,j=j,value=ifelse(.[[j]]=="",NA,.[[j]]));.}
### in.req ----
dt_final=copy(dt) %>% 
  .[,.(Date_received=Date,
       Partner=Department,
       RefNum_external=CV.No,
       Analyst=Team.in.Charge)]
dbAppendTable(conn,"in.req",dt_final)
### subject ----
dt_final=copy(dt) %>% 
  .[,.(RefNum_external=CV.No,
       Title,
       Name=NNUA,
       ID=IC.No.,
       Company,
       Designation)]
dbAppendTable(conn,"subject",dt_final)
### out.resp ----
dt_final=copy(dt) %>% 
  .[,.(Decision=Decision.Proposal,
       Remarks,
       RefNum_internal=Internal.Running.No,
       Date.responded=Date.submitted)]
dbAppendTable(conn,"out.resp",dt_final)
dbDisconnect(conn)

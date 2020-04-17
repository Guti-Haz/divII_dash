pacman::p_load(RSQLite,pool,DBI,magrittr,data.table)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# create conn
conn=dbConnect(SQLite(),"data.sqlite")
# create table
## in_req ----
tblName="in_req"
if(dbExistsTable(conn,tblName)){
  dbRemoveTable(conn,tblName)
}
c(Correspondence="TEXT",
  Counterparty="TEXT",
  RefNum_external="TEXT",
  Date_received="TEXT",
  Analyst="TEXT",
  Offence="TEXT",
  OffenceDesc="TEXT",
  LawProv="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
## out_resp ----
tblName="out_resp"
if(dbExistsTable(conn,tblName)){
  dbRemoveTable(conn,tblName)
}
c(RefNum_external="TEXT",
  RefNum_internal="TEXT",
  Date_responsed="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
# disconnect conn
dbDisconnect(conn)
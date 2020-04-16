pacman::p_load(RSQLite,pool,DBI,magrittr,data.table)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
# create conn
conn=dbConnect(SQLite(),"data.sqlite")
# create table
## in_req
tblName="in_req"
if(dbExistsTable(conn,tblName)){
  dbRemoveTable(conn,tblName)
}
c(Correspondence="TEXT",
  Counterparty="TEXT",
  RefNum_external="TEXT",
  Date_received="TEXT",
  RefNum_internal="TEXT",
  Date_responsed="TEXT",
  Analyst="TEXT",
  .ASSIGN="TEXT",
  .RESPONSE="TEXT",
  .EDIT="TEXT",
  .DELETE="TEXT") %>% 
  dbCreateTable(conn,tblName,.)
# disconnect conn
dbDisconnect(conn)
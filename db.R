
# create conn + data
if(file.exists(data_name)){file.remove(data_name)}
conn=dbConnect(SQLite(),data_name)

# create table
## subject ----
tblName="subject"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Nature="TEXT",
  RefNum_external="TEXT",
  RefNum_internal="TEXT",
  Type_Natural="INTEGER",
  Type_Legal="INTEGER",
  Name="TEXT",
  ID="TEXT",
  Account="TEXT") %>% 
  dbCreateTable(conn,tblName,.)

## in_req ----
tblName="in_req"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Correspondence="TEXT",
  Counterparty="TEXT",
  RefNum_external="TEXT",
  Date_received="TEXT",
  Analyst="TEXT",
  Offence="TEXT",
  OffenceDesc="TEXT",
  LawProv="TEXT") %>% 
  dbCreateTable(conn,tblName,.)

## valid ----
tblName="valid"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(RefNum_external="TEXT",
  Date_ask="TEXT",
  Reason="TEXT",
  Detail_inquiry="TEXT",
  Date_reply="TEXT",
  Detail_response="TEXT") %>% 
  dbCreateTable(conn,tblName,.)

## out_resp ----
tblName="out_resp"
if(dbExistsTable(conn,tblName)){dbRemoveTable(conn,tblName)}
c(Supervisor="TEXT",
  Workfile="TEXT",
  Complexity="TEXT",
  OtherInfo="TEXT",
  Offence="TEXT",
  OffenceDesc="TEXT",
  Law_Prov="TEXT",
  RefNum_external="TEXT",
  RefNum_internal="TEXT",
  Date_responsed="TEXT") %>% 
  dbCreateTable(conn,tblName,.)

# disconnect conn
dbDisconnect(conn)
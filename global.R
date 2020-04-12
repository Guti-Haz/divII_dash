corr_type1_list=c("Local LEA",
                  "Foreign FIU - ESW",
                  "AGC - MLA") %>% 
  sort
corr_type2_list=c("Incoming Request",
                  "Outgoing Request",
                  "Incoming Spontaneous",
                  "Outgoing Spontaneous") %>% 
  sort
data_counterparty_list_localLEA=c("IMI",
                                  "MACC",
                                  "RMC",
                                  "RMP - E5",
                                  "RMP - E8") %>% 
  sort
data_counterparty_list_ffiu=c("STRO",
                              "FinCEN",
                              "AUSTRAC",
                              "AMLO",
                              "JAFIC") %>% 
  sort
data_counterparty_list_agc=c("Nigeria",
                             "Ghana",
                             "Senegal",
                             "South Africa",
                             "Bhutan") %>% 
  sort
assign_list=c("MFF",
              "NHZ",
              "THL",
              "AFA",
              "MNAK")
genRandom=function(){
  runif(1,1e7,1e8) %>% 
    round(0) %>% 
    as.character
}
makeButton=function(type,ref){
  js=paste0('Shiny.setInputValue(\"',type,'_button\",this.id,{priority:"event"})')
  paste0(type,"_",ref) %>% 
    actionButton(.,label="Update",onclick=js,style="display:inline-block") %>% 
    as.character
}
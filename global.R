corr_list=c("Local",
            "ESW",
            "Special - MLA")
nature_list=c("Incoming Request",
              "Outgoing Request",
              "Incoming Spontaneous",
              "Outgoing Spontaneous") %>% 
  sort
lea_list=c("IMI",
           "MACC",
           "RMC",
           "RMP - E5",
           "RMP - E8") %>% 
  sort
country_list=c("Nigeria",
               "Ghana",
               "Senegal",
               "South Africa",
               "Bhutan") %>% 
  sort
analyst_list=c("MFF",
              "NHZ",
              "THL",
              "AFA",
              "MNAK") %>% 
  sort
supervisor_list=c("ALBA",
                  "LHH",
                  "LWL") %>% 
  sort
crime=tribble(
  ~law_provision, ~offence,
  "Penal Code 1999: 11 - Attempted murder", "Murder",
  "Income Tax Act 2011: 201 - Willfull evasion", "Tax evasion",
  "Income Tax Act 2011: 202 - Hiding info", "Tax evasion",
  "Penal Code 1999: 17 - Initiating bribery act", "Corruption",
  "Corruption Act 2001: 4 - Accepting gratification", "Corruption",
  "Corruption Act 2001: 5 - Giving gratification", "Corruption"
) %>% 
  setDT
offence_list=c("Fraud",
               "Murder",
               "Tax evasion",
               "Terrorism financing",
               "Proliferation financing") %>% 
  sort
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
css.button="color: #fff; background-color: #337ab7; border-color: #2e6da4;"
pool=dbPool(drv=RSQLite::SQLite(),dbname="data.sqlite")

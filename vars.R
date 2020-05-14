prime_start="15-10-2019"
task_list=c(
  "Incoming request",
  "Outgoing request",
  "Incoming sharing",
  "Outgoing sharing",
  "Bulk disclosure",
  "CV",
  "Adhoc",
  "SR",
  "Manual STR")
nature_list=c("Domestic",
              "Egmont",
              "MLA")
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
  ~law,~prov, ~offence,
  "Penal Code 1999","11 - Attempted murder", "Murder",
  "Income Tax Act 2011","201 - Willfull evasion", "Tax evasion",
  "Income Tax Act 2011","202 - Hiding info", "Tax evasion",
  "Penal Code 1999","17 - Initiating bribery act", "Corruption",
  "Corruption Act 2001","4 - Accepting gratification", "Corruption",
  "Corruption Act 2001","5 - Giving gratification", "Corruption"
) %>% 
  setDT
offence_list=c("Fraud",
               "Murder",
               "Tax evasion",
               "Terrorism financing",
               "Proliferation financing") %>% 
  sort
complexity_list=c("Simple",
                  "Bulk",
                  "Complex")%>% 
  sort
otherInfo_list=c("ITIS",
                 "Account statement",
                 "Credit info",
                 "Remittances")%>% 
  sort
reason_list=c("No link to Malaysia",
              "No personal info",
              "No detail of transaction",
              "Phishing")
css.button="color: #fff; background-color: #337ab7; border-color: #2e6da4;"
css.inline1="display:inline-block;vertical-align:top;"
css.inline2="display:inline-block;vertical-align:top;margin-top:26px"
css.inline3="display:inline-block;vertical-align:middle;"
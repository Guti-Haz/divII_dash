libs=c("rstudioapi",
       "shiny",
       "magrittr",
       "openxlsx",
       "janitor",
       "data.table",
       "DT",
       "DBI",
       "zoo",
       "RSQLite",
       "readxl",
       "openxlsx",
       "stringr",
       "lubridate",
       "tibble",
       "glue",
       "pool",
       "rhandsontable")
lapply(libs,require,character.only=T)
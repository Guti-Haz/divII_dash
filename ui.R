libs=c("shiny",
       "RSQLite",
       "DBI",
       "magrittr",
       "data.table",
       "DT",
       "stringr",
       "lubridate",
       "tibble",
       "glue",
       "pool",
       "rhandsontable")
lapply(libs,require,character.only=T)
source("vars.R")
shinyUI(
    fluidPage(
        h4("Dashboard - Div II"),br(),
        tabsetPanel(id="tabSet",
                    tabPanel("Add new",value="add",br(),
                             radioButtons("add_corr","Correspondence",corr_list),
                             radioButtons("add_nature","Nature",""),
                             actionButton("add_button","",icon("plus"),style=css.button)),
                    tabPanel("Task manager",value="task",br(),
                             tabsetPanel(id="tabSet2",
                                         tabPanel(title=uiOutput("title.in_req"),
                                                  DTOutput("tbl.in_req")
                                         )
                             )
                             
                    )
        )
    )
)

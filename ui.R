source("loadLibs.R")
source("vars.R")
shinyUI(
    fluidPage(
        h4("Dashboard - Div II"),br(),
        tabsetPanel(id="tabSet",
                    tabPanel("Add",value="add",br(),
                             fluidPage(fluidRow(actionButton("in.req","Incoming request",style=css.button))),br(),
                             fluidPage(fluidRow(actionButton("out.req","Outgoing request",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("in.spon","Incoming sharing",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("out.spon","Outgoing sharing",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("bulk","Bulk disclosure",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("cv","Character vetting",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("adhoc","Adhoc/Project",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("sr","Suspicious report",style=css.button))),br(),
                             # fluidPage(fluidRow(actionButton("manualSTR","Manual STR",style=css.button)))
                    ),
                    tabPanel("Task",value="task",br(),
                             tabsetPanel(id="task_tabSet",
                                         tabPanel(title=uiOutput("in.req_count"),br(),DTOutput("in.req_DT"),value="in.req"),
                                         tabPanel(title=uiOutput("out.req_count"),br(),DTOutput("out.req_DT"),value="out.req")
                             )
                    )
        )
    )
)

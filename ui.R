source("loadLibs.R")
source("vars.R")
shinyUI(
    fluidPage(
        h4("Dashboard - Div II"),br(),
        tabsetPanel(id="tabSet",
                    tabPanel("Add",value="action",br(),
                             fluidPage(fluidRow(actionButton("in.req","Incoming request",style=css.button))),br(),
                             fluidPage(fluidRow(actionButton("out.req","Outgoing request",style=css.button))),br(),
                             fluidPage(fluidRow(actionButton("out.spon","Outgoing spontaneous",style=css.button))),br(),
                             fluidPage(fluidRow(actionButton("in.spon","Incoming spontaneous",style=css.button)))
                    ),
                    tabPanel(title=uiOutput("task_count"),value="task",br(),
                             tabsetPanel(id="task_tabSet",
                                         tabPanel(title=uiOutput("in.req_count"),value="in.req",br(),
                                                  wellPanel(
                                                      radioButtons("in.req_filter_validity","Validity",choiceNames=c("Yes","No"),choiceValues=c(T,F),selected=T),
                                                      sliderInput("in.req_filter_daysElapsed","Days elapsed",1,100,1,width="300px")
                                                  ),
                                                  DTOutput("in.req_DT")
                                         ),
                                         tabPanel(title=uiOutput("out.req_count"),value="out.req",br(),
                                                  DTOutput("out.req_DT")
                                         ),
                                         tabPanel(title=uiOutput("in.spon_count"),value="in.spon",br(),
                                                  DTOutput("in.spon_DT")
                                         )
                             )
                    )
        )
    )
)

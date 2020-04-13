# for local deploy
pacman::p_load(shiny,magrittr,data.table,DT,rstudioapi,stringr,lubridate,tibble,rhandsontable)
setwd(dirname(getSourceEditorContext()$path))
source("global.R")
# # for shinyapps.io
# library(shiny)
# library(magrittr)
# library(data.table)
# library(DT)
# library(stringr)
# library(lubridate)
# library(tibble)
# library(rhandsontable)
# source("global.R")

ui=fluidPage(
    h4("Dashboard - Div II"),br(),
    tabsetPanel(id="tabSet",
                tabPanel("Task manager",br(),
                         actionButton("add_button","",icon("plus"),style=css.button),br(),
                         DTOutput("table")
                ),
                tabPanel("View"),
                tabPanel("Download")
    )
)

server=function(input, output,session){
    valToday=reactive({
        invalidateLater(4.32e+7,session) #every 12 hours
        today()
    })
    
    observeEvent(input$add_button,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="tabSet2",
                                tabPanel("Basic",br(),
                                         selectInput("add_corr","Correspondence",c("",corr_list)),
                                         selectInput("add_nature","Nature",""),
                                         selectInput("add_counterparty","Counterparty",""),
                                         textInput("add_refNum","Reference No.",value=genRandom()),
                                         dateInput("add_date","Date",value=valToday()),
                                         selectInput("add_analyst","Analyst",c("",analyst_list)),
                                         selectInput("add_supervisor","Supervisor",c("",supervisor_list))
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("add_offence","Offence",c("",sort(unique(crime$offence)))),
                                         textInput("add_offenceDesc","Offence description"),
                                         selectizeInput("add_law-prov","Law and provision","",width='400px',multiple=T)
                                ),
                                tabPanel("Intel",br(),
                                         #names/id/accounts
                                )
                    )
                ),
                actionButton("add_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )
    })
    
    observe({
        req(input$add_corr)
        if (input$add_corr=="Local"){
            updateSelectInput(session,"add_nature",choices=nature_list[c(1,4)])
            updateSelectInput(session,"add_counterparty",choices=lea_list)
        }
        if (input$add_corr=="ESW"){
            updateSelectInput(session,"add_nature",choices=nature_list)
            updateSelectInput(session,"add_counterparty",choices=country_list)
        }
        if (input$add_corr=="Special - MLA"){
            updateSelectInput(session,"add_nature",choices=nature_list[1])
            updateSelectInput(session,"add_counterparty",choices=country_list)
        }
    })
    
    observe({
        req(input$add_offence)
        updateSelectizeInput(session,"add_law-prov",
                             choices=crime[offence==input$add_offence,sort(law_provision)],server=T)
    })
    
    dt=reactiveVal(data.table())
    
    observeEvent(input$add_submit,{
        dt_new=data.table(
            Correspondence=input$add_corr,
            Nature=input$add_nature,
            Counterparty=input$add_counterparty,
            RefNum1=input$add_refNum,
            Date1=as.character(input$add_date),
            RefNum2="",
            Date2="",
            Analyst=input$add_analyst) %>% 
            .[Analyst=="",Update_Assign:=makeButton("assign",RefNum1)] %>%
            .[Nature=="Incoming Request",Update_Response:=makeButton("response",RefNum1)]
        dt_merged=rbindlist(list(dt(),dt_new),fill=T)
        dt(dt_merged)
        removeModal(session)
    })
    
    observeEvent(input$response_button,{
        showModal(
            modalDialog(
                textInput("response_refNum","Reference No.",value=genRandom()),
                dateInput("response_date","Date",value=valToday()),
                actionButton("response_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )  
    })
    
    observeEvent(input$response_submit,{
        row=str_replace(input$response_button,"response_","")
        dt_new=dt()[RefNum1==row,`:=`(RefNum2=input$response_refNum,
                                      Date2=as.character(input$response_date),
                                      Update_Response=NA)]
        dt(dt_new)
        removeModal(session)
    })
    
    observeEvent(input$assign_button,{
        showModal(
            modalDialog(
                selectInput("assign_analyst","Analyst",analyst_list),
                actionButton("assign_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )  
    })
    
    observeEvent(input$assign_submit,{
        row=str_replace(input$assign_button,"assign_","")
        dt_new=dt()[RefNum1==row,`:=`(Analyst=input$assign_analyst,
                                      Update_Assign=NA)]
        dt(dt_new)
        removeModal(session)
    })
    
    tableDT=eventReactive({
        input$assign_submit
        input$response_submit
        input$add_submit},{
            dt()
        })
    
    output$table=renderDT(
        tableDT() %>%
            DT::datatable(
                extensions="FixedHeader",
                rownames=F,
                escape=F,
                options=list(
                    processing=T,
                    autowidth=T,
                    fixedHeader=T,
                    pageLength=1e4,
                    dom="t"
                )
            )
    )
}

shinyApp(ui=ui,server=server)
#rsconnect::deployApp(dirname(rstudioapi::getSourceEditorContext()$path),launch.browser=F,forceUpdate=T)
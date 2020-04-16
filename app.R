pacman::p_load(shiny,magrittr,data.table,DT,rstudioapi,stringr,lubridate,tibble,glue,pool)
setwd(dirname(getSourceEditorContext()$path))
source("db.R")
source("global.R")

ui=fluidPage(
    h4("Dashboard - Div II"),br(),
    tabsetPanel(id="tabSet",
                tabPanel("Task manager",br(),
                         actionButton("add_button","",icon("plus"),style=css.button),br(),br(),
                         tabsetPanel(id="tabSet2",
                                     tabPanel(title=uiOutput("title.in_req"),
                                              DTOutput("tbl.in_req")
                                     ),
                                     tabPanel("Outgoing request",
                                              DTOutput("tbl.out_req")
                                     ),
                                     tabPanel("Incoming spontaneous",
                                              DTOutput("tbl.in_spon")
                                     ),
                                     tabPanel("Outgoing spontaneous",
                                              DTOutput("tbl.out_spon")
                                     )
                         )
                         
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
                    tabsetPanel(id="tabSet3",
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
                fade=F,footer=NULL,easyClose=T
            )
        )
    })
    
    observeEvent(input$add_close,{
        removeModal(session)
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
    
    observeEvent(input$add_submit,{
        if(input$add_nature=="Incoming Request"){
            data.table(
                Correspondence=input$add_corr,
                Counterparty=input$add_counterparty,
                RefNum_external=input$add_refNum,
                Date_received=as.character(input$add_date),
                RefNum_internal="",
                Date_responsed="",
                Analyst=input$add_analyst) %>% 
                dbWriteTable(pool,name="in_req",value=.,append=T)
        }
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
        glue('UPDATE in_req 
             SET 
             RefNum_internal="{input$response_refNum}",
             Date_responsed="{as.character(input$response_date)}" 
             WHERE RefNum_external="{row}"') %>% 
            dbExecute(pool,.)
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
        glue('UPDATE in_req 
             SET 
             Analyst="{input$assign_analyst}" 
             WHERE RefNum_external="{row}"') %>% 
            dbExecute(pool,.)
        removeModal(session)
    })
    
    observeEvent(input$delete_button,{
        row=str_replace(input$delete_button,"delete_","")
        glue('DELETE FROM in_req 
             WHERE RefNum_external="{row}"') %>% 
            dbExecute(pool,.)
    })
    
    rv.in_req=eventReactive({
        input$add_submit
        input$response_submit
        input$assign_submit
        input$delete_button},{
            data=dbReadTable(pool,"in_req") %>% 
                setDT %>% 
                .[Analyst=="",.ASSIGN:=sapply(RefNum_external,function(x){makeButton("assign",x,"pen")})] %>% 
                .[Date_responsed=="",.RESPONSE:=sapply(RefNum_external,function(x){makeButton("response",x,"pen")})] %>% 
                .[,.DELETE:=sapply(RefNum_external,function(x){makeButton("delete",x,"trash")})] %>% 
                .[,.EDIT:=sapply(RefNum_external,function(x){makeButton("edit",x,"wrench")})]
            return(data)
        },ignoreNULL=F)
    
    output$title.in_req=renderText({
        pending=rv.in_req()[Analyst==""|Date_responsed=="",.N]
        data=HTML(glue("Incoming request <font color='red'> <b>({pending})</b></font>"))
        return(data)
    })
    
    output$tbl.in_req=renderDT(
        rv.in_req() %>% 
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
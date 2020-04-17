pacman::p_load(shiny,magrittr,data.table,DT,rstudioapi,stringr,lubridate,tibble,glue,pool)
setwd(dirname(getSourceEditorContext()$path))
source("db.R")
source("global.R")

ui=fluidPage(
    h4("Dashboard - Div II"),br(),
    tabsetPanel(id="tabSet",
                tabPanel("Add new",br(),
                         radioButtons("add_corr","Correspondence",corr_list),
                         radioButtons("add_nature","Nature",""),
                         actionButton("add_button","",icon("plus"),style=css.button)),
                tabPanel("Task manager",br(),
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
        if(input$add_nature=="in_req"){
            showModal(
                modalDialog(
                    fluidPage(
                        tabsetPanel(id="tabSet3",
                                    tabPanel("Meta",br(),
                                             selectInput("add_counterparty","Counterparty",""),
                                             textInput("add_refNum","Reference No.",value=genRandom()),
                                             dateInput("add_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                             selectInput("add_analyst","Analyst",c("",analyst_list))
                                    ),
                                    tabPanel("Crime",br(),
                                             selectInput("add_offence","Offence",c("",sort(unique(crime$offence)))),
                                             textInput("add_offenceDesc","Offence description"),
                                             selectizeInput("add_lawNprov","Law and provision","",width='400px',multiple=T)
                                    ),
                                    tabPanel("Subject",br(),
                                             #names/id/accounts
                                    )
                        )
                    ),
                    actionButton("add_submit","",icon("check-circle"),style=css.button),
                    fade=F,footer=NULL,easyClose=T
                )
            )   
        }
        if (input$add_corr=="Local"){updateSelectInput(session,"add_counterparty",choices=lea_list)}
        if (input$add_corr=="ESW"){updateSelectInput(session,"add_counterparty",choices=country_list)}
        if (input$add_corr=="Special - MLA"){updateSelectInput(session,"add_counterparty",choices=country_list)}
    })
    
    observeEvent(input$add_close,{
        removeModal(session)
    })
    
    observe({
        req(input$add_corr)
        if (input$add_corr=="Local"){updateRadioButtons(session,"add_nature",choiceNames=names(nature_list[c(1,4)]),choiceValues=unname(nature_list[c(1,4)]))}
        if (input$add_corr=="ESW"){updateRadioButtons(session,"add_nature",choiceNames=names(nature_list),choiceValues=unname(nature_list))}
        if (input$add_corr=="Special - MLA"){updateRadioButtons(session,"add_nature",choiceNames=names(nature_list[1]),choiceValues=unname(nature_list[1]))}
    })
    
    observe({
        req(input$add_offence)
        updateSelectizeInput(session,"add_lawNprov",
                             choices=crime[offence==input$add_offence,sort(law_provision)],server=T)
    })

    observeEvent(input$add_submit,{
        if(input$add_nature=="in_req"){
            data=c(Correspondence=input$add_corr,
              Counterparty=input$add_counterparty,
              RefNum_external=input$add_refNum,
              Date_received=format(input$add_date,"%d-%m-%Y"),
              Analyst=input$add_analyst,
              Offence=input$add_offence,
              OffenceDesc=input$add_offenceDesc,
              LawProv=paste0(input$add_lawNprov,collapse=","))
            sprintf('INSERT INTO %s (%s) VALUES (%s)',
                    input$add_nature,
                    paste0(names(data),collapse=","),
                    paste0(paste0("'",data,"'"),collapse=",")) %>% 
                dbExecute(pool,.)
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
        data=c(RefNum_external=str_replace(input$response_button,"response_",""),
               RefNum_internal=input$response_refNum,
               Date_responsed=format(input$response_date,"%d-%m-%Y"))
        sprintf('INSERT INTO out_resp (%s) VALUES (%s)',
                paste0(names(data),collapse=","),
                paste0(paste0("'",data,"'"),collapse=",")) %>% 
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
        glue('UPDATE in_req SET 
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
            data=setDT(merge(dbReadTable(pool,"in_req"),dbReadTable(pool,"out_resp"),by="RefNum_external",all.x=T)) %>% 
                .[is.na(Date_responsed),DayPassed:=as.integer(today()-dmy(Date_received))] %>% 
                .[,`:=`(.ASSIGN=character(),.RESPONSE=character(),.DELETE=character())] %>% 
                .[Analyst=="",.ASSIGN:=sapply(RefNum_external,function(x){makeButton("assign",x,"pen")})] %>%
                .[is.na(Date_responsed),.RESPONSE:=sapply(RefNum_external,function(x){makeButton("response",x,"pen")})] %>%
                .[,.DELETE:=sapply(RefNum_external,function(x){makeButton("delete",x,"trash")})] %>% 
                .[,.(
                    Counterparty,
                    RefNum_external,
                    Date_received,
                    DayPassed,
                    Date_responsed,
                    Analyst,
                    .ASSIGN,
                    .RESPONSE,
                    .DELETE
                )]
            return(data)
        },ignoreNULL=F)
    
    output$title.in_req=renderText({
        pending=rv.in_req()[is.na(Analyst)|is.na(Date_responsed),.N]
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
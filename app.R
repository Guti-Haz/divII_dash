pacman::p_load(shiny,magrittr,data.table,DT,rstudioapi,stringr,lubridate,tibble,glue,pool,rhandsontable)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# library(shiny)
# library(RSQLite)
# library(DBI)
# library(magrittr)
# library(data.table)
# library(DT)
# library(stringr)
# library(lubridate)
# library(tibble)
# library(glue)
# library(pool)
# library(rhandsontable)

source("db.R")
source("vars.R")
source("udf.R")

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
                                             textInput("add_offenceDesc","Offence description")
                                    ),
                                    tabPanel("Subject",br(),
                                             rHandsontableOutput("add_subject")
                                    )
                        )
                    ),br(),
                    actionButton("add_submit","",icon("check-circle"),style=css.button),
                    fade=F,footer=NULL,easyClose=T,size="l"
                )
            )   
        }
        if (input$add_corr=="Local"){updateSelectInput(session,"add_counterparty",choices=lea_list)}
        if (input$add_corr=="ESW"){updateSelectInput(session,"add_counterparty",choices=country_list)}
        if (input$add_corr=="Special - MLA"){updateSelectInput(session,"add_counterparty",choices=country_list)}
    })
    
    output$add_subject=renderRHandsontable({
        rhandsontable(
            data.table(
                Type_Natural=T,
                Type_Legal=F,
                Name="",
                ID="",
                Account=""),
            fillHandle=list(direction='vertical',autoInsertRow=T),
            rowHeaderWidth=c(30,30,100,100,100)
        )
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

    observeEvent(input$add_submit,{
        if(input$add_nature=="in_req"){
            data=c(Correspondence=input$add_corr,
                   Counterparty=input$add_counterparty,
                   RefNum_external=input$add_refNum,
                   Date_received=format(input$add_date,"%d-%m-%Y"),
                   Analyst=input$add_analyst,
                   Offence=input$add_offence,
                   OffenceDesc=input$add_offenceDesc)
            sprintf('INSERT INTO %s (%s) VALUES (%s)',
                    input$add_nature,
                    paste0(names(data),collapse=","),
                    paste0(paste0("'",data,"'"),collapse=",")) %>% 
                dbExecute(pool,.)
            data=hot_to_r(input$add_subject) %>% 
                adjTypetoDB %>% 
                .[,`:=`(
                    Nature=input$add_nature,
                    RefNum_external=input$add_refNum,
                    RefNum_internal=NA
                )] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(pool,"subject",data,append=T)
        }
        removeModal(session)
    })
    
    observeEvent(input$response_button,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="tabSet4",
                                tabPanel("Meta",br(),
                                         radioButtons("response_supervisor","Supervisor",supervisor_list,inline=T),
                                         textInput("response_refNum","Reference No.",value=genRandom()),
                                         dateInput("response_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                         textInput("response_wf","Workfile",value=glue("WF/{year(valToday())}/0000")),
                                         radioButtons("response_complexity","Complexity",complexity_list,inline=T),
                                         selectizeInput("response_otherInfo","Other info disclosed",otherInfo_list,width='400px',multiple=T)
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("response_offence","Offence",c("",sort(unique(crime$offence)))),
                                         textInput("response_offenceDesc","Offence description"),
                                         selectizeInput("response_lawNprov","Law and provision","",width='400px',multiple=T),
                                ),
                                tabPanel("Subject",br(),
                                         rHandsontableOutput("response_subject")
                                )
                    )
                ),br(),
                actionButton("response_submit","",icon("check-circle"),style=css.button),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )  
    })
    
    observe({
        req(input$response_offence)
        updateSelectizeInput(session,"response_lawNprov",
                             choices=crime[offence==input$response_offence,sort(law_provision)],server=T)
    })
    
    output$response_subject=renderRHandsontable({
        data=setDT(dbReadTable(pool,"subject")) %>% 
            .[RefNum_external==str_replace(input$response_button,"response_","")] %>% 
            adjDBtoType %>% 
            .[,.(Type_Natural,Type_Legal,Name,ID,Account)]
        rhandsontable(
            data,
            fillHandle=list(direction='vertical',autoInsertRow=T),
            rowHeaderWidth=c(30,30,100,100,100)
        )
    })
    
    observeEvent(input$response_submit,{
        data=c(Supervisor=input$response_supervisor,
               Workfile=input$response_wf,
               Complexity=input$response_complexity,
               OtherInfo=paste0(input$response_otherInfo,collapse=","),
               Offence=input$response_offence,
               OffenceDesc=input$response_offenceDesc,
               Law_Prov=paste0(input$response_lawNprov,collapse=","),
               RefNum_external=str_replace(input$response_button,"response_",""),
               RefNum_internal=input$response_refNum,
               Date_responsed=format(input$response_date,"%d-%m-%Y"))
        sprintf('INSERT INTO out_resp (%s) VALUES (%s)',
                paste0(names(data),collapse=","),
                paste0(paste0("'",data,"'"),collapse=",")) %>% 
            dbExecute(pool,.)
        data=hot_to_r(input$response_subject) %>% 
            .[,`:=`(
                Nature="out_resp",
                RefNum_external=str_replace(input$response_button,"response_",""),
                RefNum_internal=input$response_refNum
            )] %>% 
            .[!(is.na(Name)&is.na(ID)&is.na(Account))]
        dbWriteTable(pool,"subject",data,append=T)
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
# rsconnect::deployApp()

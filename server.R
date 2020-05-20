source("udf.R")
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
corr.pool=dbPool(drv=RSQLite::SQLite(),dbname="corr.sqlite")
cv.pool=dbPool(drv=RSQLite::SQLite(),dbname="cv.sqlite")
shinyServer(function(input,output,session){
    
    RV=reactiveValues(
        in.req=in.req_fx(corr.pool,T,1),
        out.req=out.req_fx(corr.pool),
        in.spon=in.spon_fx(corr.pool)
    )
    
    valToday=reactive({
        invalidateLater(4.32e+7,session) #every 12 hours
        today()
    })
# task_count ----

    output$task_count=renderText({
        v1=RV$in.req[is.na(Analyst)|is.na(Date_responsed),.N]
        v2=RV$out.req[is.na(Date_reply),.N]
        v3=RV$in.spon[is.na(Analyst),.N]
        data=HTML(glue("Task <font color='red'> <b>({v1+v2+v3})</b></font>"))
        return(data)
    })
    
# incoming request ----
    
    ## add
    
    observeEvent(input$in.req,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="in.req_add_tabSet",
                                tabPanel("Meta",br(),
                                         radioButtons("in.req_add_nature","Nature",nature_list,selected=character(0),inline=T),
                                         selectInput("in.req_add_partner",NULL,NULL),
                                         div(style=css.inline1,textInput("in.req_add_refNum","Reference no.")),
                                         div(style=css.inline2,actionButton("in.req_add_gen",NULL,icon("chrome"),style=css.button)),
                                         dateInput("in.req_add_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                         selectInput("in.req_add_analyst","Analyst",c("",analyst_list))
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("in.req_add_offence","Offence",c("",sort(unique(crime$offence)))),
                                         textInput("in.req_add_offenceDesc","Offence description")
                                ),
                                tabPanel("Subject",br(),
                                         rHandsontableOutput("in.req_add_subject"),br(),br(),
                                         div(style=css.inline3,actionButton("in.req_add_submit",NULL,icon("check-circle"),style=css.button)),
                                         div(style=css.inline3,uiOutput("in.req_add_val"))
                                )
                    )
                ),br(),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )
        output$in.req_add_subject=renderRHandsontable({
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
    })

    observeEvent(input$in.req_add_nature,{
        if (input$in.req_add_nature=="Domestic"){
            updateSelectInput(session,"in.req_add_partner","Agency",choices=lea_list,selected=character(0))
        }
        if (input$in.req_add_nature%in%c("Egmont","MLA")){
            updateSelectInput(session,"in.req_add_partner","Country",choices=country_list,selected=character(0))
        }
    })
    
    observeEvent(input$in.req_add_gen,{
        updateTextInput(session,"in.req_add_refNum",value=genRandom())})
    
    observeEvent(input$in.req_add_submit,{
        check=list(
            Nature=!is.null(input$in.req_add_nature),
            Partner=!input$in.req_add_partner%in%c(NULL,""),
            RefNum=input$in.req_add_refNum!="",
            Date=dmy(format(input$in.req_add_date,"%d-%m-%Y"))<=valToday(),
            Offence=input$in.req_add_offence!="",
            Subject=hot_to_r(input$in.req_add_subject)[,Name]!=""    
        )
        output$in.req_add_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            data=c(Nature=input$in.req_add_nature,
                   Partner=input$in.req_add_partner,
                   RefNum_external=input$in.req_add_refNum,
                   Date_received=format(input$in.req_add_date,"%d-%m-%Y"),
                   Analyst=input$in.req_add_analyst,
                   Offence_received=input$in.req_add_offence,
                   OffenceDesc_received=input$in.req_add_offenceDesc)
            ingest(corr.pool,"in_req",data)
            data=hot_to_r(input$in.req_add_subject) %>% 
                adjTypetoDB %>% 
                .[,`:=`(Nature=input$in.req_add_nature,
                        Type="in.req",
                        RefNum_external=input$in.req_add_refNum)] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(corr.pool,"subject",data,append=T)
            removeModal(session)
            output$in.req_add_val=renderUI({NULL})
            RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
        }
    })
    
    ## assign
    
    observeEvent(input$in.req_assign_button,{
        showModal(
            modalDialog(
                selectInput("in.req_assign_analyst","Analyst",analyst_list),
                actionButton("in.req_assign_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )  
    })
    
    observeEvent(input$in.req_assign_submit,{
        row=str_replace(input$in.req_assign_button,"in.req_assign_","")
        data=c(Analyst=input$in.req_assign_analyst)
        update(corr.pool,"in_req",data,row)
        removeModal(session)
        RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
    })
    
    ## invalidate
    
    observeEvent(input$in.req_invalidate_button,{
        showModal(
            modalDialog(
                dateInput("in.req_invalidate_date","Date",value=valToday(),format="dd-mm-yyyy"),
                selectizeInput("in.req_invalidate_reason","Reason",reason_list,width='400px',multiple=T),
                textAreaInput("in.req_invalidate_detail","Detail"),
                actionButton("in.req_invalidate_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )
    })
    
    observeEvent(input$in.req_invalidate_submit,{
        row=str_replace(input$in.req_invalidate_button,"in.req_invalidate_","")
        data=c(RefNum_external=row,
               Date_ask=format(input$in.req_invalidate_date,"%d-%m-%Y"),
               Reason=paste0(input$in.req_invalidate_reason,collapse=","),
               Detail_inquiry=input$in.req_invalidate_detail)
        ingest(corr.pool,"valid",data)
        removeModal(session)
        RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
    })
    
    ## revalidate
    
    observeEvent(input$in.req_revalidate_button,{
        showModal(
            modalDialog(
                dateInput("in.req_revalidate_date","Date",value=valToday(),format="dd-mm-yyyy"),
                textAreaInput("in.req_revalidate_detail","Detail"),
                actionButton("in.req_revalidate_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )
    })
    
    observeEvent(input$in.req_revalidate_submit,{
        row=str_replace(input$in.req_revalidate_button,"in.req_revalidate_","")
        data=c(Date_reply=format(input$in.req_revalidate_date,"%d-%m-%Y"),
               Detail_response=input$in.req_revalidate_detail)
        update(corr.pool,"valid",data,row)
        removeModal(session)
        RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
    })
    
    ## response
    
    observeEvent(input$in.req_response_button,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="response_in.req_tabSet",
                                tabPanel("Meta",br(),
                                         radioButtons("in.req_response_supervisor","Supervisor",supervisor_list,selected=character(0),inline=T),
                                         div(style=css.inline1,textInput("in.req_response_refNum","Reference No")),
                                         div(style=css.inline2,actionButton("in.req_response_gen",NULL,icon("chrome"),style=css.button)),
                                         dateInput("in.req_response_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                         textInput("in.req_response_wf","Workfile",value=glue("WF/{year(valToday())}/")),
                                         radioButtons("in.req_response_complexity","Complexity",complexity_list,selected=character(0),inline=T),
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("in.req_response_offence","Offence",c("",sort(unique(crime$offence)))),
                                         textInput("in.req_response_offenceDesc","Offence description"),
                                         selectizeInput("in.req_response_law","Law",NULL),
                                         selectizeInput("in.req_response_prov","Provision",NULL)
                                ),
                                tabPanel("Intel",br(),
                                         radioButtons("in.req_response_anyIntel","Any intel?",choiceNames=c("Yes","No"),choiceValues=c(T,F),selected=T,inline=T),
                                         textAreaInput("in.req_response_str","STR",height="100px"),
                                         selectizeInput("in.req_response_otherInfo","Other info",otherInfo_list,width='400px',multiple=T)
                                ),
                                tabPanel("Subject",br(),
                                         rHandsontableOutput("in.req_response_subject"),br(),br(),
                                         div(style=css.inline3,actionButton("in.req_response_submit",NULL,icon("check-circle"),style=css.button)),
                                         div(style=css.inline3,uiOutput("in.req_response_val"))
                                )
                    )
                ),br(),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )
        output$in.req_response_subject=renderRHandsontable({
            data=setDT(dbReadTable(corr.pool,"subject")) %>% 
                .[RefNum_external==str_replace(input$in.req_response_button,"in.req_response_","")] %>% 
                adjDBtoType %>% 
                .[,.(Type_Natural,Type_Legal,Name,ID,Account)]
            rhandsontable(
                data,
                fillHandle=list(direction='vertical',autoInsertRow=T),
                rowHeaderWidth=c(30,30,100,100,100)
            )
        })
    })

    observeEvent(input$in.req_response_gen,{
        updateTextInput(session,"in.req_response_refNum",value=genRandom())
    })
    
    observeEvent(input$in.req_response_offence,{
        temp=crime[offence==input$in.req_response_offence,c("",sort(law))]
        updateSelectizeInput(session,"in.req_response_law",choices=temp,server=T)
    })
    
    observeEvent(input$in.req_response_law,{
        temp=crime[offence==input$in.req_response_offence&law==input$in.req_response_law,c("",sort(prov))]
        updateSelectizeInput(session,"in.req_response_prov",choices=temp,server=T)
    })
    
    observeEvent(input$in.req_response_submit,{
        row=str_replace(input$in.req_response_button,"in.req_response_","")
        check=list(Supervisor=!is.null(input$in.req_response_supervisor),
                   RefNum=input$in.req_response_refNum!="",
                   Date=dmy(format(input$in.req_response_date,"%d-%m-%Y"))<=valToday(),
                   Workfile=str_detect(input$in.req_response_wf,"WF/\\d{4}/\\d{4}"),
                   Complexity=!is.null(input$in.req_response_complexity),
                   Offence=input$in.req_response_offence!="",
                   Law=!(is.null(input$in.req_response_law)|input$in.req_response_law==""),
                   Provision=!(is.null(input$in.req_response_prov)|input$in.req_response_prov==""),
                   STR=(input$in.req_response_anyIntel==T&input$in.req_response_str!="")|(input$in.req_response_anyIntel==F&input$in.req_response_str==""))
        output$in.req_response_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            # write to out_resp
            data=c(Supervisor=input$in.req_response_supervisor,
                   Workfile=input$in.req_response_wf,
                   Complexity=input$in.req_response_complexity,
                   AnyIntel=input$in.req_response_anyIntel,
                   OtherInfo=paste0(input$in.req_response_otherInfo,collapse=","),
                   Offence_responsed=input$in.req_response_offence,
                   OffenceDesc_responsed=input$in.req_response_offenceDesc,
                   Law=input$in.req_response_law,
                   Prov=input$in.req_response_prov,
                   RefNum_external=str_replace(input$in.req_response_button,"in.req_response_",""),
                   RefNum_internal=input$in.req_response_refNum,
                   Date_responsed=format(input$in.req_response_date,"%d-%m-%Y"))
            ingest(corr.pool,"out_resp",data)
            # write to subject
            data=hot_to_r(input$in.req_response_subject) %>% 
                .[,`:=`(Nature=setDT(dbReadTable(corr.pool,"in_req"))[RefNum_external==row,Nature],
                        Type="out_resp",
                        RefNum_external=row,
                        RefNum_internal=input$in.req_response_refNum)] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(corr.pool,"subject",data,append=T)
            # write to str_tbl
            data=data.table(Nature=setDT(dbReadTable(corr.pool,"in_req"))[RefNum_external==row,Nature],
                            Type="out_resp",
                            RefNum_external=row,
                            RefNum_internal=input$in.req_response_refNum,
                            STR=unlist(str_split(input$in.req_response_str,"\n")))
            dbWriteTable(corr.pool,"str_tbl",data,append=T)
            removeModal(session)   
            RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
        }
    })
    
    ## task
    
    output$in.req_count=renderText({
        pending=RV$in.req[is.na(Analyst)|is.na(Date_responsed),.N]
        data=HTML(glue("Incoming request <font color='red'> <b>({pending})</b></font>"))
        return(data)
    })
    
    observeEvent(input$in.req_filter_validity,{
        RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
    })
    
    observeEvent(input$in.req_filter_daysElapsed,{
        RV$in.req=in.req_fx(corr.pool,input$in.req_filter_validity,input$in.req_filter_daysElapsed)
    })
    
    output$in.req_DT=renderDT({
        RV$in.req %>% 
            DT::datatable(
                extensions=c("FixedHeader","Buttons"),
                rownames=F,
                escape=F,
                options=list(
                    processing=T,
                    autowidth=T,
                    fixedHeader=T,
                    pageLength=1e4,
                    dom="FBrti",
                    buttons=c('copy','csv','excel','pdf')
                )
            )
    })


# outgoing request ----
    
    ## add
    
    observeEvent(input$out.req,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="out.req_add_tabSet",
                                tabPanel("Meta",br(),
                                         selectInput("out.req_add_partner","Country",c("",country_list)),
                                         selectInput("out.req_add_analyst","Analyst",c("",analyst_list)),
                                         radioButtons("out.req_add_supervisor","Supervisor",supervisor_list,selected=character(0),inline=T),
                                         selectInput("out.req_add_onBehalf","On behalf of",c("",lea_list)),
                                         textInput("out.req_add_leaOfficer","Officer name"),
                                         textInput("out.req_add_refNum","Reference No"),
                                         dateInput("out.req_add_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                         textInput("out.req_add_wf","Workfile",value=glue("WF/{year(valToday())}/")),
                                         radioButtons("out.req_add_complexity","Complexity",complexity_list,selected=character(0),inline=T),
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("out.req_add_offence","Offence",sort(unique(crime$offence)),selected=character(0)),
                                         textInput("out.req_add_offenceDesc","Offence description"),
                                         selectizeInput("out.req_add_law","Law",NULL),
                                         selectizeInput("out.req_add_prov","Provision",NULL)
                                ),
                                tabPanel("Intel",br(),
                                         textAreaInput("out.req_add_str","STR",height="100px"),
                                         selectizeInput("out.req_add_otherInfo","Other info",otherInfo_list,width='400px',multiple=T)
                                ),
                                tabPanel("Subject",br(),
                                         rHandsontableOutput("out.req_add_subject"),br(),br(),
                                         div(style=css.inline3,actionButton("out.req_add_submit",NULL,icon("check-circle"),style=css.button)),
                                         div(style=css.inline3,uiOutput("out.req_add_val"))
                                )
                    )
                ),br(),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )
        output$out.req_add_subject=renderRHandsontable({
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
    })
    
    observeEvent(input$out.req_add_offence,{
        temp=crime[offence==input$out.req_add_offence,c("",sort(law))]
        updateSelectizeInput(session,"out.req_add_law",choices=temp,server=T)
    })
    
    observeEvent(input$out.req_add_law,{
        temp=crime[offence==input$out.req_add_offence&law==input$out.req_add_law,c("",sort(prov))]
        updateSelectizeInput(session,"out.req_add_prov",choices=temp,server=T)
    })
    
    observeEvent(input$out.req_add_submit,{
        check=list(Partner=!input$out.req_add_partner%in%c(NULL,""),
                   Analyst=input$out.req_add_analyst!="",
                   Supervisor=!is.null(input$out.req_add_supervisor),
                   onBehalf=input$out.req_add_onBehalf!="",
                   leaOfficer=input$out.req_add_leaOfficer!="",
                   RefNum=input$out.req_add_refNum!="",
                   Date=dmy(format(input$out.req_add_date,"%d-%m-%Y"))<=valToday(),
                   Workfile=str_detect(input$out.req_add_wf,"WF/\\d{4}/\\d{4}"),
                   Complexity=!is.null(input$out.req_add_complexity),
                   Offence=input$out.req_add_offence!="",
                   Law=!(is.null(input$out.req_add_law)|input$out.req_add_law==""),
                   Provision=!(is.null(input$out.req_add_prov)|input$out.req_add_prov==""),
                   Subject=hot_to_r(input$out.req_add_subject)[,Name]!="")
        output$out.req_add_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            # write to out_resp
            data=c(Partner=input$out.req_add_partner,
                   Analyst=input$out.req_add_analyst,
                   Supervisor=input$out.req_add_supervisor,
                   OnBehalf=input$out.req_add_onBehalf,
                   LeaOfficer=input$out.req_add_leaOfficer,
                   RefNum_internal=input$out.req_add_refNum,
                   Date_ask=format(input$out.req_add_date,"%d-%m-%Y"),
                   Workfile=input$out.req_add_wf,
                   Complexity=input$out.req_add_complexity,
                   Offence_asked=input$out.req_add_offence,
                   OffenceDesc_asked=input$out.req_add_offenceDesc,
                   Law=input$out.req_add_law,
                   Prov=input$out.req_add_prov,
                   OtherInfo=paste0(input$out.req_add_otherInfo,collapse=","))
            ingest(corr.pool,"out_req",data)
            # write to subject
            data=hot_to_r(input$out.req_add_subject) %>% 
                .[,`:=`(Nature="Egmont",
                        Type="out_req",
                        RefNum_internal=input$out.req_add_refNum)] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(corr.pool,"subject",data,append=T)
            # write to str_tbl
            data=data.table(Nature="Egmont",
                            Type="out_req",
                            RefNum_internal=input$out.req_add_refNum,
                            STR=unlist(str_split(input$out.req_add_str,"\n")))
            dbWriteTable(corr.pool,"str_tbl",data,append=T)
            removeModal(session)   
            output$out.req_val=renderUI({NULL})
            RV$out.req=out.req_fx(corr.pool)
        }
    })
    
    ## response
    
    observeEvent(input$out.req_response_button,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="response_out.req_tabSet",
                                tabPanel("Meta",br(),
                                         div(style=css.inline1,textInput("out.req_response_refNum","Reference No")),
                                         div(style=css.inline2,actionButton("out.req_response_gen",NULL,icon("chrome"),style=css.button)),
                                         dateInput("out.req_response_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                         div(style=css.inline3,actionButton("out.req_response_submit",NULL,icon("check-circle"),style=css.button)),
                                         div(style=css.inline3,uiOutput("out.req_response_val"))
                                )
                    )
                ),br(),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )
    })
    
    observeEvent(input$out.req_response_gen,{
        updateTextInput(session,"out.req_response_refNum",value=genRandom())
    })
    
    observeEvent(input$out.req_response_submit,{
        row=str_replace(input$out.req_response_button,"out.req_response_","")
        check=list(RefNum=input$out.req_response_refNum!="",
                   Date=dmy(format(input$out.req_response_date,"%d-%m-%Y"))<=valToday())
        output$out.req_response_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            # write to in_resp
            data=c(RefNum_internal=row,
                   RefNum_external=input$out.req_response_refNum,
                   Date_reply=format(input$out.req_response_date,"%d-%m-%Y"))
            ingest(corr.pool,"in_resp",data)
            removeModal(session)
            RV$out.req=out.req_fx(corr.pool)
        }
    })
    
    ## task
    
    output$out.req_count=renderText({
        pending=RV$out.req[is.na(Date_reply),.N]
        data=HTML(glue("Outgoing request <font color='red'> <b>({pending})</b></font>"))
        return(data)
    })
    
    output$out.req_DT=renderDT({
        RV$out.req %>% 
            DT::datatable(
                extensions=c("FixedHeader","Buttons"),
                rownames=F,
                escape=F,
                options=list(
                    processing=T,
                    autowidth=T,
                    fixedHeader=T,
                    pageLength=1e4,
                    dom="FBrti",
                    buttons=c('copy','csv','excel','pdf')
                )
            )
    })
    
# outgoing sharing ----
    
    ## add
    
    observeEvent(input$out.spon,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="out.spon_add_tabSet",
                                tabPanel("Meta",br(),
                                         radioButtons("out.spon_add_nature","Nature",nature_list2,selected=character(0),inline=T),
                                         selectInput("out.spon_add_partner",NULL,NULL),
                                         selectInput("out.req_add_analyst","Analyst",c("",analyst_list)),
                                         radioButtons("out.spon_add_supervisor","Supervisor",supervisor_list,selected=character(0),inline=T),
                                         div(style=css.inline1,textInput("out.spon_add_refNum","Reference no.")),
                                         div(style=css.inline2,actionButton("out.spon_add_gen",NULL,icon("chrome"),style=css.button)),
                                         dateInput("out.spon_add_date","Date",value=valToday(),format="dd-mm-yyyy"),
                                         textInput("out.spon_add_wf","Workfile",value=glue("WF/{year(valToday())}/")),
                                         radioButtons("out.spon_add_complexity","Complexity",complexity_list,selected=character(0),inline=T)
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("out.spon_add_offence","Offence",sort(unique(crime$offence)),selected=character(0)),
                                         textInput("out.spon_add_offenceDesc","Offence description"),
                                         selectizeInput("out.spon_add_law","Law",NULL),
                                         selectizeInput("out.spon_add_prov","Provision",NULL)
                                ),
                                tabPanel("Intel",br(),
                                         textAreaInput("out.spon_add_str","STR",height="100px"),
                                         selectizeInput("out.spon_add_otherInfo","Other info",otherInfo_list,width='400px',multiple=T)
                                ),
                                tabPanel("Subject",br(),
                                         rHandsontableOutput("out.spon_add_subject"),br(),br(),
                                         actionButton("out.spon_add_submit",NULL,icon("check-circle"),style=css.button),
                                         actionButton("out.spon_add_submit.repop",NULL,icon("align-center"),style=css.button),br(),br(),
                                         uiOutput("out.spon_add_val")
                                )
                    )
                ),br(),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )
    })
    
    observeEvent(input$out.spon_add_nature,{
        if (input$out.spon_add_nature=="Domestic"){
            updateSelectInput(session,"out.spon_add_partner","Agency",choices=lea_list,selected=character(0))
        }
        if (input$out.spon_add_nature=="Egmont"){
            updateSelectInput(session,"out.spon_add_partner","Country",choices=country_list,selected=character(0))
        }
    })
    
    observeEvent(input$out.spon_add_offence,{
        temp=crime[offence==input$out.spon_add_offence,c("",sort(law))]
        updateSelectizeInput(session,"out.spon_add_law",choices=temp,server=T)
    })
    
    observeEvent(input$out.spon_add_law,{
        temp=crime[offence==input$out.spon_add_offence&law==input$out.spon_add_law,c("",sort(prov))]
        updateSelectizeInput(session,"out.spon_add_prov",choices=temp,server=T)
    })
    
    output$out.spon_add_subject=renderRHandsontable({
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
    
    observeEvent(input$out.spon_add_submit,{
        check=list(Nature=!is.null(input$out.spon_add_nature),
                   Partner=!input$out.spon_add_partner%in%c(NULL,""),
                   Analyst=input$out.spon_add_analyst!="",
                   Supervisor=!is.null(input$out.spon_add_supervisor),
                   RefNum=input$out.spon_add_refNum!="",
                   Date=dmy(format(input$out.spon_add_date,"%d-%m-%Y"))<=valToday(),
                   Workfile=str_detect(input$out.spon_add_wf,"WF/\\d{4}/\\d{4}"),
                   Complexity=!is.null(input$out.spon_add_complexity),
                   Offence=input$out.spon_add_offence!="",
                   Law=!(is.null(input$out.spon_add_law)|input$out.spon_add_law==""),
                   Provision=!(is.null(input$out.spon_add_prov)|input$out.spon_add_prov==""),
                   Subject=hot_to_r(input$out.spon_add_subject)[,Name]!="")
        output$out.spon_add_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            # write to out_resp
            data=c(Nature=input$out.spon_add_nature,
                   Partner=input$out.spon_add_partner,
                   Analyst=input$out.spon_add_analyst,
                   Supervisor=input$out.spon_add_supervisor,
                   RefNum=input$out.spon_add_refNum,
                   Date=format(input$out.spon_add_date,"%d-%m-%Y"),
                   Workfile=input$out.spon_add_wf,
                   Complexity=input$out.spon_add_complexity,
                   Offence=input$out.spon_add_offence,
                   OffenceDesc=input$out.spon_add_offenceDesc,
                   Law=input$out.spon_add_law,
                   Prov=input$out.spon_add_prov,
                   OtherInfo=paste0(input$out.spon_add_otherInfo,collapse=","))
            ingest(corr.pool,"out_spon",data)
            # write to subject
            data=hot_to_r(input$out.spon_add_subject) %>% 
                .[,`:=`(Nature=input$out.spon_add_nature,
                        Type="out_spon",
                        RefNum_internal=input$out.spon_add_refNum)] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(corr.pool,"subject",data,append=T)
            # write to str_tbl
            data=data.table(Nature=input$out.spon_add_nature,
                            Type="out_spon",
                            RefNum_internal=input$out.spon_add_refNum,
                            STR=unlist(str_split(input$out.spon_add_str,"\n")))
            dbWriteTable(corr.pool,"str_tbl",data,append=T)
            removeModal(session)   
            output$out.spon_val=renderUI({NULL})
        }
    })
    
    observeEvent(input$out.spon_add_submit.repop,{
        check=list(Nature=!is.null(input$out.spon_add_nature),
                   Partner=!input$out.spon_add_partner%in%c(NULL,""),
                   Analyst=input$out.spon_add_analyst!="",
                   Supervisor=!is.null(input$out.spon_add_supervisor),
                   RefNum=input$out.spon_add_refNum!="",
                   Date=dmy(format(input$out.spon_add_date,"%d-%m-%Y"))<=valToday(),
                   Workfile=str_detect(input$out.spon_add_wf,"WF/\\d{4}/\\d{4}"),
                   Complexity=!is.null(input$out.spon_add_complexity),
                   Offence=input$out.spon_add_offence!="",
                   Law=!(is.null(input$out.spon_add_law)|input$out.spon_add_law==""),
                   Provision=!(is.null(input$out.spon_add_prov)|input$out.spon_add_prov==""),
                   Subject=hot_to_r(input$out.spon_add_subject)[,Name]!="")
        output$out.spon_add_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            # write to out_resp
            data=c(Nature=input$out.spon_add_nature,
                   Partner=input$out.spon_add_partner,
                   Analyst=input$out.spon_add_analyst,
                   Supervisor=input$out.spon_add_supervisor,
                   RefNum=input$out.spon_add_refNum,
                   Date=format(input$out.spon_add_date,"%d-%m-%Y"),
                   Workfile=input$out.spon_add_wf,
                   Complexity=input$out.spon_add_complexity,
                   Offence=input$out.spon_add_offence,
                   OffenceDesc=input$out.spon_add_offenceDesc,
                   Law=input$out.spon_add_law,
                   Prov=input$out.spon_add_prov,
                   OtherInfo=paste0(input$out.spon_add_otherInfo,collapse=","))
            ingest(corr.pool,"out_spon",data)
            # write to subject
            data=hot_to_r(input$out.spon_add_subject) %>% 
                .[,`:=`(Nature=input$out.spon_add_nature,
                        Type="out_spon",
                        RefNum_internal=input$out.spon_add_refNum)] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(corr.pool,"subject",data,append=T)
            # write to str_tbl
            data=data.table(Nature=input$out.spon_add_nature,
                            Type="out_spon",
                            RefNum_internal=input$out.spon_add_refNum,
                            STR=unlist(str_split(input$out.spon_add_str,"\n")))
            dbWriteTable(corr.pool,"str_tbl",data,append=T)
            output$out.spon_add_val=renderUI({successUI()})
        }
    })
    
# incoming sharing ----
    
    ## add
    observeEvent(input$in.spon,{
        showModal(
            modalDialog(
                fluidPage(
                    tabsetPanel(id="in.spon_add_tabSet",
                                tabPanel("Meta",br(),
                                         selectInput("in.spon_add_partner","Country",country_list),
                                         div(style=css.inline1,textInput("in.spon_add_refNum","Reference no.")),
                                         div(style=css.inline2,actionButton("in.spon_add_gen",NULL,icon("chrome"),style=css.button)),
                                         dateInput("in.spon_add_date","Date",value=valToday(),format="dd-mm-yyyy")
                                ),
                                tabPanel("Crime",br(),
                                         selectInput("in.spon_add_offence","Offence",c("",sort(unique(crime$offence)))),
                                         textInput("in.spon_add_offenceDesc","Offence description")
                                ),
                                tabPanel("Subject",br(),
                                         rHandsontableOutput("in.spon_add_subject"),br(),br(),
                                         div(style=css.inline3,actionButton("in.spon_add_submit",NULL,icon("check-circle"),style=css.button)),
                                         div(style=css.inline3,uiOutput("in.spon_add_val"))
                                )
                    )
                ),br(),
                fade=F,footer=NULL,easyClose=T,size="l"
            )
        )
        output$in.spon_add_subject=renderRHandsontable({
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
    })
    
    observeEvent(input$in.spon_add_gen,{
        updateTextInput(session,"in.spon_add_refNum",value=genRandom())})
    
    observeEvent(input$in.spon_add_submit,{
        check=list(
            Partner=!input$in.spon_add_partner%in%c(NULL,""),
            RefNum=input$in.spon_add_refNum!="",
            Date=dmy(format(input$in.spon_add_date,"%d-%m-%Y"))<=valToday(),
            Offence=input$in.spon_add_offence!="",
            Subject=hot_to_r(input$in.spon_add_subject)[,Name]!=""    
        )
        output$in.spon_add_val=renderUI({errorUI(check)})
        if(do.call(all,check)){
            data=c(Partner=input$in.spon_add_partner,
                   RefNum_external=input$in.spon_add_refNum,
                   Date=format(input$in.spon_add_date,"%d-%m-%Y"),
                   Offence=input$in.spon_add_offence,
                   OffenceDesc=input$in.spon_add_offenceDesc)
            ingest(corr.pool,"in_spon",data)
            data=hot_to_r(input$in.spon_add_subject) %>% 
                adjTypetoDB %>% 
                .[,`:=`(Nature="Egmont",
                        Type="in.spon",
                        RefNum_external=input$in.spon_add_refNum)] %>% 
                .[!(is.na(Name)&is.na(ID)&is.na(Account))]
            dbWriteTable(corr.pool,"subject",data,append=T)
            removeModal(session)
            output$in.spon_add_val=renderUI({NULL})
            RV$in.spon=in.spon_fx(corr.pool)
        }
    })
    
    ## assign
    
    observeEvent(input$in.spon_assign_button,{
        showModal(
            modalDialog(
                selectInput("in.spon_assign_analyst","Analyst",analyst_list),
                actionButton("in.spon_assign_submit","",icon("check-circle"),style=css.button),
                easyClose=T,footer=NULL
            )
        )  
    })
    
    observeEvent(input$in.spon_assign_submit,{
        row=str_replace(input$in.spon_assign_button,"in.spon_assign_","")
        data=c(Analyst=input$in.spon_assign_analyst)
        update(corr.pool,"in_spon",data,row)
        removeModal(session)
        RV$in.spon=in.spon_fx(corr.pool)
    })
    
    ## task
    
    output$in.spon_count=renderText({
        pending=RV$in.spon[is.na(Analyst),.N]
        data=HTML(glue("Incoming spontaneous <font color='red'> <b>({pending})</b></font>"))
        return(data)
    })

    output$in.spon_DT=renderDT({
        RV$in.spon %>% 
            DT::datatable(
                extensions=c("FixedHeader","Buttons"),
                rownames=F,
                escape=F,
                options=list(
                    processing=T,
                    autowidth=T,
                    fixedHeader=T,
                    pageLength=1e4,
                    dom="FBrti",
                    buttons=c('copy','csv','excel','pdf')
                )
            )
    })
    
})
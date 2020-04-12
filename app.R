pacman::p_load(shiny,magrittr,data.table,DT,rstudioapi,stringr)
setwd(dirname(getSourceEditorContext()$path))
source("global.R")

ui=fluidPage(
    tabsetPanel(id="tabSet",
                tabPanel("Main",
                         DTOutput("table")
                ),
                tabPanel("Data input",
                         uiOutput("inputList")
                )
    )
)

server=function(input, output,session){
    observe({
        if(input$tabSet=="Data input"){
            showModal(
                modalDialog(
                    selectInput("corr_type1","Counterparty",corr_type1_list,size=3,selectize=F),
                    selectInput("corr_type2","Nature","",size=4,selectize=F),
                    actionButton("corr_submit","Submit")
                )
            )   
        }
    })
    
    observeEvent(input$corr_type1,{
        if (input$corr_type1=="Local LEA"){temp=corr_type2_list[c(1,4)]}
        if (input$corr_type1=="Foreign FIU - ESW"){temp=corr_type2_list}
        if (input$corr_type1=="AGC - MLA"){temp=corr_type2_list[1]}
        updateSelectInput(session,"corr_type2",choices=temp)
    })
    
    observeEvent(input$corr_submit,{
        removeModal(session)
        output$inputList=renderUI({
            tagList(
                selectInput("data_counterparty","Counterparty",""),
                textInput("data_refNum","Reference No.",value=genRandom()),
                dateInput("data_date","Date"),
                actionButton("data_submit","Submit")
            )
        })
        if(input$corr_type1=="Local LEA"){temp=data_counterparty_list_localLEA}
        if(input$corr_type1=="Foreign FIU - ESW"){temp=data_counterparty_list_ffiu}
        if(input$corr_type1=="AGC - MLA"){temp=data_counterparty_list_agc}
        updateSelectInput(session,"data_counterparty",choices=temp)
    })
    
    dt=reactiveVal(data.table())
    
    observeEvent(input$data_submit,{
        output$inputList=renderUI({""})
        dt_new=data.table(
            Type=input$corr_type1,
            Nature=input$corr_type2,
            Counterparty=input$data_counterparty,
            RefNum1=input$data_refNum,
            Date1=input$data_date) %>% 
            .[,Assign:=makeButton("Assign",RefNum1)] %>%  #manage button
            .[Nature=="Incoming Request",Response:=makeButton("Response",RefNum1)] #manage button
        dt_merged=rbindlist(list(dt(),dt_new),fill=T)
        dt(dt_merged)
        updateTabsetPanel(session,"tabSet",selected="Main")
    })
    
    observeEvent(input$Response_button,{
        showModal(
            modalDialog(
                textInput("response_refNum","Reference No.",value=genRandom()),
                dateInput("response_date","Date"),
                actionButton("response_submit","Submit")
            )
        )  
    })
    
    observeEvent(input$response_submit,{
        selectedRefNum1=str_replace(input$Response_button,"Response_","")
        dt_new=dt() %>% 
            .[RefNum1==selectedRefNum1,`:=`(RefNum2=input$response_refNum,
                                            Date2=input$response_date,
                                            Response=NA)] #manage button
        dt(dt_new)
        removeModal(session)
    })
    
    observeEvent(input$Assign_button,{
        showModal(
            modalDialog(
                selectInput("assign_analyst","Analyst",assign_list),
                actionButton("assign_submit","Submit")
            )
        )  
    })
    
    observeEvent(input$assign_submit,{
        selectedRefNum1=str_replace(input$Assign_button,"Assign_","")
        dt_new=dt() %>% 
            .[RefNum1==selectedRefNum1,`:=`(Assignee=input$assign_analyst,
                                            Assign=NA)] #manage button
        dt(dt_new)
        removeModal(session)
    })
    
    tableDT=eventReactive({
        input$assign_submit
        input$response_submit
        input$corr_submit},{
            dt()
        })
    
    output$table=renderDT(
        tableDT(),
        escape=F
    )
}

shinyApp(ui=ui,server=server)

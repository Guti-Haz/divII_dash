## Search

rv.search.in_req=eventReactive({
  input$in.req_submit
  input$assign_submit
  input$invalidate_submit
  input$revalidate_submit
  input$response_submit
  input$delete_button},{
    in_req=setDT(dbReadTable(pool,"in_req")) %>% 
      .[dmy(Date_received)>=dmy(prime_start)]
    out_resp=setDT(dbReadTable(pool,"out_resp")) %>% 
      .[!is.na(RefNum_external)]
    valid=setDT(dbReadTable(pool,"valid")) %>% 
      .[!(!is.na(Date_ask)&!is.na(Date_reply))]
    data=merge(in_req,out_resp,by="RefNum_external",all.x=T) %>% 
      merge(valid,by="RefNum_external",all.x=T) %>% 
      setDT %>% 
      .[,.(
        RefNum_external,
        Nature,
        Partner,
        Date_received,
        Analyst,
        RefNum_internal,
        Date_responsed,
        Date_ask,
        Date_reply
      )]
    return(data)
  },ignoreNULL=F)

output$tbl.search.in_req=renderDT({
  rv.search.in_req() %>% 
    DT::datatable(
      filter = 'top',
      extensions=c("FixedHeader","Buttons"),
      rownames=F,
      escape=F,
      options=list(
        processing=T,
        autowidth=T,
        fixedHeader=T,
        pageLength=30,
        dom="FBrti",
        buttons=c('copy','csv','excel','pdf')
      )
    )
})

rv.search_subject=eventReactive({
  input$in.req_submit
  input$response_submit},{
    data=setDT(dbReadTable(pool,"subject")) %>% 
      .[,.(Nature,
           RefNum_external,
           Name,
           ID,
           Account)]
    return(data)
  },ignoreNULL=F)

output$tbl.search.subject=renderDT({
  rv.search_subject() %>% 
    DT::datatable(
      filter = 'top',
      extensions=c("FixedHeader","Buttons"),
      rownames=F,
      escape=F,
      options=list(
        processing=T,
        autowidth=T,
        fixedHeader=T,
        pageLength=30,
        dom="FBrti",
        buttons=c('copy','csv','excel','pdf')
      )
    )
})
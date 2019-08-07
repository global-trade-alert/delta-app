library(xlsx)
library(gtalibrary)
library(zoo)
library(data.table)
library(splitstackshape)
library(plyr)
library(stringr)
library(data.table)
library(DT)

source(paste0(app.path,'/functions/table_filter.R'))
source(paste0(app.path,'/functions/xlsx_query.R'))

server <- function(input, output, session) {
  
  RV <- reactiveValues(display = data.frame(start.df='start.display'),
                       xlsx.query = NULL,
                       clear.xlsx = FALSE)
  
  #reset excel input field and feed its data through xlsx.query
  observe({
    req(input$excel.query)
    req(!RV$clear.xlsx)
    RV$xlsx.query <- read.xlsx(input$excel.query$datapath, sheetIndex = 1)
  })
  
  observeEvent(input$excel.query, {
    RV$clear.xlsx <- FALSE
  }, priority = 1000)
  
  observeEvent(input$clear.xlsx, {
    RV$xlsx.query <- NULL
    RV$clear.xlsx <- TRUE
    reset('excel.query')
  }, priority = 1000)
  
  observeEvent(input$submit.query, {
    
    if(is.null(RV$xlsx.query)){
      RV$display <- table_filter(input$hs.codes,
                               input$cpc.codes,
                               input$implementing.jurisdiction,
                               input$affected.jurisdiction,
                               input$start.date,
                               input$end.date,
                               input$regime.type,
                               input$regime.name,
                               input$tariff.unit,
                               input$display.previous.tariff,
                               input$sort.liberalised.restricted)

      
    } else {
      RV$display <- xlsx_query(RV$xlsx.query,
                               input$xlsx.sort)
      
    }
    })
  
  #create output tabs depending on length of list to display
  #and if sort by restricted/liberalised is checked, create sub tabs (this is for the request when enterring new tariffs to be able to show which are red/green/amber)
  observeEvent(input$submit.query, {
  
  if((input$sort.liberalised.restricted==T&is.null(RV$xlsx.query))|
     (input$xlsx.sort==T&!is.null(RV$xlsx.query))){ 

  show("sorted.table.outputs")
  hide("table.outputs")
    
  output$sorted.table.outputs <- renderUI({
    nTabs = length(RV$display)
    # create tabPanel with datatable in it
    myTabs = lapply(seq_len(nTabs), function(i) {
      tabPanel(paste0("Query n° ",i),
               uiOutput(paste0("datatable_",i))
      )
    })

    do.call(tabsetPanel, myTabs)
  })

    lapply(seq_len(length(RV$display)), function(i) {
      output[[paste0("datatable_",i)]] <- renderUI({
        tabsetPanel(id=paste0("subdatatable_",i),
                    tabPanel("Liberalised",
                             DT::dataTableOutput(paste0("datatable_",i,"_1"))),
                    tabPanel("Restricted",
                             DT::dataTableOutput(paste0("datatable_",i,"_2"))),
                    tabPanel("Amber",
                             DT::dataTableOutput(paste0("datatable_",i,"_3")))
        )
      })
      
      output[[paste0("datatable_",i,"_1")]] <- DT::renderDataTable(
        exp=subset(as.data.frame(RV$display[[i]]),Evaluation=='Liberalised',
                   select=names(as.data.frame(RV$display[[i]]))!='Evaluation'),
        rownames = F,
        options = list(
          pageLength = 30,
          scrollX='400px',
          dom = 'Brtip',
          stateSave = TRUE,
          language = list(
            zeroRecords = "Blabla no rows remain for selected criterion")
        ),
        server=FALSE
      )
      
      output[[paste0("datatable_",i,"_2")]] <- DT::renderDataTable(
        exp=subset(as.data.frame(RV$display[[i]]),Evaluation=='Restricted',
                   select=names(as.data.frame(RV$display[[i]]))!='Evaluation'),
        rownames = F,
        options = list(
          pageLength = 30,
          scrollX='400px',
          dom = 'Brtip',
          stateSave = TRUE,
          language = list(
            zeroRecords = "Blabla no rows remain for selected criterion")
        ),
        server=FALSE
      )
      
      output[[paste0("datatable_",i,"_3")]] <- DT::renderDataTable(
        exp=subset(as.data.frame(RV$display[[i]]),Evaluation=='Amber',
                   select=names(as.data.frame(RV$display[[i]]))!='Evaluation'),
        rownames = F,
        options = list(
          pageLength = 30,
          scrollX='400px',
          dom = 'Brtip',
          stateSave = TRUE,
          language = list(
            zeroRecords = "Blabla no rows remain for selected criterion")
        ),
        server=FALSE
      )
      
    })
  } else {
    
    hide("sorted.table.outputs")
    show("table.outputs")
    
    output$table.outputs <- renderUI({
      nTabs = length(RV$display)
      myTabs = lapply(seq_len(nTabs), function(i) {
        tabPanel(paste0("Query n° ",i),
                 DT::dataTableOutput(paste0("unsorted_datatable_",i))
        )
      })
      
      do.call(tabsetPanel, myTabs)
    })
    
      lapply(seq_len(length(RV$display)), function(i) {
        output[[paste0("unsorted_datatable_",i)]] <- DT::renderDataTable(
          exp=as.data.frame(RV$display[[i]]),
          rownames = F,
          options = list(
            pageLength = 30,
            scrollX='400px',
            dom = 'Brtip',
            stateSave = TRUE,
            language = list(
              zeroRecords = "Blabla no rows remain for selected criterion")
          ),
          server=FALSE
        )
      })
    
  }
    
  })

  #save as xlsx
  output$dl <- downloadHandler(
    filename = function() {paste0(Sys.Date(),' query.xlsx')},
    content = function(file) {
      for(i in 1:length(RV$display)){
        if(nrow(as.data.frame(RV$display[[i]]))==0){
          write.xlsx(data.frame(nothing.remains='no results for your query'), file, 
                                                sheetName=paste0('Query n° ',i),append=T,row.names=F)
          }else{
        write.xlsx(as.data.frame(RV$display[[i]]), file, 
                   sheetName=paste0('Query n° ',i),append=T,row.names=F)
          }
      }  
    } 
  )
  
  #track parameter choices in console
  observeEvent(input$submit.query, {
    test<<-RV$display
    if(input$affected.jurisdiction=='Any'){affected.jurisdiction='any'}else{affected.jurisdiction=paste(input$implementing.jurisdiction,collapse=';')}
    if(input$regime.type=='Any'){regime.type='any'}else{regime.type=paste(input$regime.type,collapse=';')}
    if(input$regime.name=='Any'){regime.name='any'}else{regime.type=paste(input$regime.name,collapse=';')}
    if(is.null(input$excel.query)){excel.query='Null'}else{excel.query=input$excel.query$datapath}
    
    cat(paste0("Parameters chosen:\n\n", 
               "\tHS.codes: ",input$hs.codes,'\n',
               "\tCPC.codes: ",input$cpc.codes,'\n',
               "\tImplementing.jurisdiction: ", paste(input$implementing.jurisdiction,collapse=';'),'\n',
               "\tAffected.jurisdiction: ", affected.jurisdiction,'\n',
               "\tStart.date: ", input$start.date,'\n',
               "\tEnd.date: ", input$end.date,'\n',
               "\tRegime.type: ", regime.type,'\n',
               "\tRegime.name: ", regime.name,'\n',
               "\tExcel.name: ", excel.query,'\n'
               
               
               
    )
    )
  })
  
}  


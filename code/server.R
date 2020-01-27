server <- function(input, output, session) {
  
  
  useShinyjs()
  
  RV=reactiveValues(display=list(datasets::Orange,datasets::Orange),
                    xlsx.upload = NULL,
                    xlsx.up.rnd = 0,
                    clear.xlsx = FALSE,
                    delta.data=NULL,
                    confirm.xlsx = 0,
                    confirm.opt.cols=NULL,
                    unrecognized.variables=NULL,
                    xlsx.query = NULL,
                    xlsx.query.rnd = 0,
                    clear.query.xlsx = FALSE,
                    confirm.query.xlsx = 0,
                    xlsx.query.result = NULL)
  
  
  #reset excel input field and feed its data through xlsx.upload
  observe({
    req(input$excel.upload)
    req(!RV$clear.xlsx)
    req(RV$xlsx.up.rnd==0)
    
    if(length(grep('.xlsx',input$excel.upload$datapath)!=0)){
      RV$xlsx.upload <- read_excel(input$excel.upload$datapath, sheet = 1)
    }
    
    if(length(grep('.csv',input$excel.upload$datapath)!=0)){
      RV$xlsx.upload <- read.csv(input$excel.upload$datapath)
    }
    
    #first check with delta_confirm_xlsx
    #checks vector lengths and that necessary cols are in place
    RV$confirm.xlsx <- gta_delta_confirm_xlsx(RV$xlsx.upload, upload=T)
    #capture data to be passed on if subsequent tests succeed
    RV$delta.data <<- RV$confirm.xlsx[[3]]
    if(is.null(RV$confirm.xlsx[[1]]) | isFALSE(RV$confirm.xlsx[[1]])){
      showNotification(HTML(RV$confirm.xlsx[[2]]), duration=10)
      RV$confirm.xlsx = RV$confirm.xlsx[[3]]
      #use a rnd counter to avoid endless loop, set the rnd to 0 for initial round
      RV$xlsx.up.rnd=1
      
      #second check is more granular
      #check if all inputs are acceptable
      
      RV$confirm.xlsx = gta_delta_input_check(RV$confirm.xlsx)
      if (isFALSE(RV$confirm.xlsx[[1]])){
        show('display')
        hide('unrecognized.variables')
        
        shinyalert('All variables are interpretable too!', 
                   RV$confirm.xlsx[[2]], type = "warning", 
                   showCancelButton = T, 
                   confirmButtonText = 'Confirm Upload',
                   #on user response, if confirm and non empty input name start upload
                   callbackR = function(x) {
                     if(x==T){
                       
                       print('Upload en cours')
                       
                       if(input$upload.name==''){showNotification("upload.name was not provided and thus the upload has been aborted, try again!", duration=5)}
                         
                       if(input$upload.name!=''){
                         RV$delta.data<<-gta_delta_input_ids(RV$delta.data)
                         print("uploading")
                         
                         withProgress(message = "Uploading, don't close the app (dev: haven't yet implemented the ability
                                      to run the upload in the background upon exit from users (progress bar updates every 100k chunk of data uploaded))", value = 0, {
                           
                           for(chunk in seq(1,nrow(RV$delta.data),100000)){
                             print(paste("start chunk",chunk))
                             gta_delta_upload(delta.data = RV$delta.data[chunk:min(nrow(RV$delta.data),chunk+99999),],
                                              input.name = input$upload.name,
                                              user.id=unique(RV$delta.data$author.id))
                             
                             incProgress(100000/nrow(RV$delta.data))
                             print(paste("END chunk",chunk))
                           }
                           
                         })   
                       } 

                     }
                   })
        
      } else {
        RV$unrecognized.variables=RV$confirm.xlsx[[3]]
        
        hide('display')
        show('unrecognized.variables')
        shinyalert('Rejected variables!', 
                   HTML(RV$confirm.xlsx[[2]]), type = "error")
        click('clear.xlsx')
      }
      
    } else if (isTRUE(RV$confirm.xlsx[[1]])) {
      #if fatal=T show error popup
      shinyalert(str_extract(RV$confirm.xlsx[[2]], "[^.]+"), 
                 HTML(stri_extract_first_regex(RV$confirm.xlsx[[2]], "(?<=\\.).*")), type = "error")
      click('clear.xlsx')
    } 
    
  })
    
  output$unrecognized.variables = DT::renderDataTable(
    exp=RV$unrecognized.variables,
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
  
  observeEvent(input$excel.upload, {
    RV$clear.xlsx <- FALSE
    RV$xlsx.up.rnd=0
  }, priority = 1000)
  
  observeEvent(input$clear.xlsx, {
    RV$xlsx.upload <- NULL
    RV$clear.xlsx <- TRUE
    RV$xlsx.up.rnd=0
    reset('excel.upload')
  }, priority = 1000)
  
  
  
  #download xlsx query template 
  output$dl.xlsx.query.template <- downloadHandler(
    filename = function() {'query template.xlsx'},
    content = function(file) {
      query.template=read.xlsx('17 Shiny/6 delta app/data/query sample.xlsx', sheet=1, detectDates = T)
      write.xlsx(query.template, file)
      
      
    } 
  )
  
  #download upload template 
  output$dl.upload.template <- downloadHandler(
    filename = function() {'upload template.xlsx'},
    content = function(file) {
      upload.template=read.xlsx("17 Shiny/6 delta app/data/delta upload template.xlsx", sheet=1, detectDates = T)
      write.xlsx(upload.template, file)
      
      
    } 
  )
  
  #xlsx input query
  observe({
    req(input$excel.query)
    req(!RV$clear.query.xlsx)
    req(RV$xlsx.query.rnd==0)
    
    RV$xlsx.query <- read_excel(input$excel.query$datapath, sheet = 1)
    RV$confirm.query.xlsx <- gta_delta_confirm_xlsx(RV$xlsx.query, query=T)
    
    if(isTRUE(RV$confirm.query.xlsx)){
      RV$xlsx.query.rnd=1
      
      showNotification(HTML(RV$confirm.query.xlsx[[2]]), duration=10)
      click('clear.query.xlsx')
    } else {
      RV$xlsx.query.rnd=1
      
      showNotification(HTML('Successful Upload, query result is computing'), duration=10)
      RV$xlsx.query.result<<-RV$confirm.query.xlsx[[3]]
      RV$display<<-list(gta_delta_query(expand=F,
                                   implementer=RV$xlsx.query.result$implementing.jurisdiction,
                                   cutoff.date=RV$xlsx.query.result$cutoff.date,
                                   new.treatment.value=RV$xlsx.query.result$new.treatment.value,
                                   new.treatment.unit=RV$xlsx.query.result$new.treatment.unit,
                                   treatment.area=RV$xlsx.query.result$treatment.area,
                                   treatment.code=RV$xlsx.query.result$treatment.code,
                                   treatment.code.type=RV$xlsx.query.result$treatment.code.type,
                                   affected.country=RV$xlsx.query.result$affected.jurisdiction,
                                   db.connection='pool',
                                   user=input$users))
      
      display.cols=c("Implementing jurisdiction","Affected jurisdiction","Affected code","Affected code type","Policy area","New date","New value","New unit","Prior value","Prior unit","Prior date","Match precision","Is mfn")
      
      if(all(is.na(as.data.frame(RV$display[[1]][,c("Prior value","Prior unit","Prior date")])))){
        showNotification('sadly no rows matching your query were found :(', duration=10)
      } else if(!all(is.na(as.data.frame(RV$display[[1]])[,c('New value','New unit')]))){
        showElement('sorted.display')
        hideElement('unsorted.display')
        
        output$sorted.display <- renderUI({
          nTabs = length(RV$display)
          # create tabPanel with datatable in it
          myTabs = lapply(seq_len(nTabs), function(i) {
            tabPanel(paste0("Query n\u00B0 ",i),
                     uiOutput(paste0("datatable_",i))
            )
          })
          
          do.call(tabsetPanel, myTabs)
        })
        
        lapply(seq_len(length(RV$display)), function(i) {
          output[[paste0("datatable_",i)]] <- renderUI({
            tabsetPanel(id=paste0("subdatatable_",i),
                        tabPanel("Decrease",
                                 DT::dataTableOutput(paste0("datatable_",i,"_1"))),
                        tabPanel("Increase",
                                 DT::dataTableOutput(paste0("datatable_",i,"_2"))),
                        tabPanel("Unclear",
                                 DT::dataTableOutput(paste0("datatable_",i,"_3"))),
                        tabPanel("Unchanged",
                                 DT::dataTableOutput(paste0("datatable_",i,"_4")))
            )
          })
          
          output[[paste0("datatable_",i,"_1")]] <- DT::renderDataTable(
            exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Decrease', select=display.cols),
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
            exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Increase', select=display.cols),
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
            exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Unclear', select=display.cols),
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
          
          output[[paste0("datatable_",i,"_4")]] <- DT::renderDataTable(
            exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Unchanged', select=display.cols),
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
        showElement('unsorted.display')
        hideElement('sorted.display')
        
        output$unsorted.display <- renderUI({
          nTabs = length(RV$display)
          myTabs = lapply(seq_len(nTabs), function(i) {
            tabPanel(paste0("Query n\u00B0 ",i),
                     DT::dataTableOutput(paste0("unsorted_datatable_",i))
            )
          })
          
          do.call(tabsetPanel, myTabs)
        })
        
        lapply(seq_len(length(RV$display)), function(i) {
          output[[paste0("unsorted_datatable_",i)]] <- DT::renderDataTable(
            exp=subset(as.data.frame(RV$display[[i]]), select=display.cols[!display.cols %in% c("New value","New unit")]),
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
    }
  })
  
  observeEvent(input$excel.query, {
    RV$clear.query.xlsx <- FALSE
    RV$xlsx.query.rnd=0
  }, priority = 1000)
  
  observeEvent(input$clear.query.xlsx, {
    RV$xlsx.query <- NULL
    RV$clear.query.xlsx <- TRUE
    RV$xlsx.query.rnd=0
    reset('excel.query')
  }, priority = 1000)
  
  #download display
  output$dl <- downloadHandler(
    filename = function() {paste0(Sys.Date(),' query.xlsx')},
    content = function(file) {
      keep.cols=c("Implementing jurisdiction","Affected jurisdiction","Affected code","Affected code type","Policy area","New date","New value","New unit","Prior value","Prior unit","Prior date","Match precision","Is mfn")
      
      if(!all(is.na(as.data.frame(RV$display[[1]][,c("New value","New unit")])))){
        
        out.list=list(Decrease=subset(as.data.frame(RV$display[[1]]), sort.result=='Decrease', select=keep.cols),
                      Increase=subset(as.data.frame(RV$display[[1]]), sort.result=='Increase', select=keep.cols),
                      Unclear=subset(as.data.frame(RV$display[[1]]), sort.result=='Unclear', select=keep.cols),
                      Unchanged=subset(as.data.frame(RV$display[[1]]), sort.result=='Unchanged', select=keep.cols))
        write.xlsx(out.list, file)

        
      
      } else {
        write.xlsx(as.data.frame(RV$display[[1]])[,keep.cols[!keep.cols %in% c("New value","New unit")]], file,
                   sheetName='Query Result')
      }
      
    } 
  )
  
  #query display
  observeEvent(input$submit.query, {
    if(length(input$date)==0) cutoff.date=NA else cutoff.date=input$date
    RV$display<<-list(display=gta_delta_query(expand=T,
                                            implementer=input$implementing.jurisdiction,
                                            cutoff.date=cutoff.date,
                                            new.treatment.value=as.numeric(input$new.treatment.value),
                                            new.treatment.unit=input$new.treatment.unit,
                                            treatment.area=input$treatment.area,
                                            treatment.code=str_trim(unlist(str_split(input$treatment.codes,','))),
                                            treatment.code.type=input$treatment.code.type,
                                            affected.country=input$affected.jurisdiction,
                                            db.connection='pool',
                                            user=input$users)
                    )
    
    display.cols=c("Implementing jurisdiction","Affected jurisdiction","Affected code","Affected code type","Policy area","New date","New value","New unit","Prior value","Prior unit","Prior date","Match precision","Is mfn")
    
    if(all(is.na(as.data.frame(RV$display[[1]][,c("Prior value","Prior unit","Prior date")])))){
      showNotification('sadly no rows matching your query were found', duration=10)
    } else if(!all(is.na(as.data.frame(RV$display[[1]])[,c('New value','New unit')]))){
    showElement('sorted.display')
    hideElement('unsorted.display')
      
    output$sorted.display <- renderUI({
      nTabs = length(RV$display)
      # create tabPanel with datatable in it
      myTabs = lapply(seq_len(nTabs), function(i) {
        tabPanel(paste0("Query n\u00B0 ",i),
                 uiOutput(paste0("datatable_",i))
        )
      })

      do.call(tabsetPanel, myTabs)
    })

    lapply(seq_len(length(RV$display)), function(i) {
      output[[paste0("datatable_",i)]] <- renderUI({
        tabsetPanel(id=paste0("subdatatable_",i),
                    tabPanel("Decrease",
                             DT::dataTableOutput(paste0("datatable_",i,"_1"))),
                    tabPanel("Increase",
                             DT::dataTableOutput(paste0("datatable_",i,"_2"))),
                    tabPanel("Unclear",
                             DT::dataTableOutput(paste0("datatable_",i,"_3"))),
                    tabPanel("Unchanged",
                             DT::dataTableOutput(paste0("datatable_",i,"_4")))
        )
      })

      output[[paste0("datatable_",i,"_1")]] <- DT::renderDataTable(
        exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Decrease', select=display.cols),
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
        exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Increase', select=display.cols),
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
        exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Unclear', select=display.cols),
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

      output[[paste0("datatable_",i,"_4")]] <- DT::renderDataTable(
        exp=subset(as.data.frame(RV$display[[i]]), sort.result=='Unchanged', select=display.cols),
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
      showElement('unsorted.display')
      hideElement('sorted.display')

      output$unsorted.display <- renderUI({
        nTabs = length(RV$display)
        myTabs = lapply(seq_len(nTabs), function(i) {
          tabPanel(paste0("Query n\u00B0 ",i),
                   DT::dataTableOutput(paste0("unsorted_datatable_",i))
          )
        })
        
        do.call(tabsetPanel, myTabs)
      })
      
      lapply(seq_len(length(RV$display)), function(i) {
        output[[paste0("unsorted_datatable_",i)]] <- DT::renderDataTable(
          exp=subset(as.data.frame(RV$display[[i]]), select=display.cols[!display.cols %in% c("New value","New unit")]),
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
}

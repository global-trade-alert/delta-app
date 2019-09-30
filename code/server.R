server <- function(input, output, session) {
  
  
  useShinyjs()
  
  RV=reactiveValues(display=list(datasets::Orange,datasets::Orange),
                    xlsx.query = NULL,
                    xlsx.rnd = 0,
                    clear.xlsx = FALSE,
                    confirm.xlsx = 0,
                    confirm.opt.cols=NULL,
                    unrecognized.variables=NULL)
  
  
  #reset excel input field and feed its data through xlsx.query
  observe({
    req(input$excel.query)
    req(!RV$clear.xlsx)
    req(RV$xlsx.rnd==0)
    
    if(length(grep('.xlsx',input$excel.query$datapath)!=0)){
    RV$xlsx.query <- read_excel(input$excel.query$datapath, sheet = 1)
    }
    
    if(length(grep('.csv',input$excel.query$datapath)!=0)){
      RV$xlsx.query <- read.csv(input$excel.query$datapath)
    }
    
    #first check with delta_confirm_xlsx
    #checks vector lengths and that necessary cols are in place
    RV$confirm.xlsx <- gta_delta_confirm_xlsx(RV$xlsx.query)
    if(is.null(RV$confirm.xlsx[[1]]) | isFALSE(RV$confirm.xlsx[[1]])){
      showNotification(HTML(RV$confirm.xlsx[[2]]), duration=10)
      RV$confirm.xlsx = RV$confirm.xlsx[[3]]
      #use a rnd counter to avoid endless loop, set the rnd to 0 for initial round
      RV$xlsx.rnd=1
      
      #second check is more granular
      #check if all inputs are acceptable
      
      RV$confirm.xlsx = gta_delta_input_check(RV$confirm.xlsx)
      if (isFALSE(RV$confirm.xlsx[[1]])){
        show('display')
        hide('unrecognized.variables')
        showNotification(paste('All variables are interpretable too!', 
                               RV$confirm.xlsx[[2]]),
                         
                         duration=10)
      } else {
        RV$unrecognized.variables=RV$confirm.xlsx[[3]]
        
        hide('display')
        show('unrecognized.variables')
        shinyalert('Rejected variables!', 
                   HTML(RV$confirm.xlsx[[2]]), type = "error")
        click('clear.xlsx')
      }
      
    } else if (isTRUE(RV$confirm.xlsx[[1]])){
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
  
  observeEvent(input$excel.query, {
    RV$clear.xlsx <- FALSE
    RV$xlsx.rnd=0
  }, priority = 1000)
  
  observeEvent(input$clear.xlsx, {
    RV$xlsx.query <- NULL
    RV$clear.xlsx <- TRUE
    RV$xlsx.rnd=0
    reset('excel.query')
  }, priority = 1000)
  
  
  
  observeEvent(input$submit.query, {
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  #query display
  observeEvent(input$submit.query, {
    RV$display=list(display=gta_delta_query(link.implementer=input$implementing.jurisdiction,
                               cutoff.date=input$end.date,
                               link.treatment.area=input$treatment.area,
                               link.affected.flow=input$affected.flow,
                               link.code=str_trim(unlist(str_split(input$treatment.codes,','))),
                               link.code.type=input$treatment.code.type,
                               link.affected.country=input$affected.jurisdiction,
                               db.connection='pool',
                               user=input$users))
    
    output$display <- renderUI({
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
                    tabPanel("Liberalised",
                             DT::dataTableOutput(paste0("datatable_",i,"_1"))),
                    tabPanel("Restricted",
                             DT::dataTableOutput(paste0("datatable_",i,"_2"))),
                    tabPanel("Amber",
                             DT::dataTableOutput(paste0("datatable_",i,"_3")))
        )
      })
      
      output[[paste0("datatable_",i,"_1")]] <- DT::renderDataTable(
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
      
      output[[paste0("datatable_",i,"_2")]] <- DT::renderDataTable(
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
      
      output[[paste0("datatable_",i,"_3")]] <- DT::renderDataTable(
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
    
  })
}

server <- function(input, output, session) {
  
  RV=reactiveValues(display=list(datasets::Orange,datasets::Orange),
                    xlsx.query = NULL,
                    xlsx.rnd = 0,
                    clear.xlsx = FALSE,
                    confirm.xlsx = 0,
                    confirm.opt.cols=NULL)
  
  
  #reset excel input field and feed its data through xlsx.query
  observe({
    req(input$excel.query)
    req(!RV$clear.xlsx)
    req(RV$xlsx.rnd==0)
    RV$xlsx.query <- read_excel(input$excel.query$datapath, sheet = 1)
    
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
        showNotification(paste('All variables are interpretable too!', 
                               RV$confirm.xlsx[[2]]),
                         
                         duration=10)
      } else {
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
  
  
  
  
  #query display
  observeEvent(input$submit.query, {
    
    
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

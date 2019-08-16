server <- function(input, output, session) {
  
  RV=reactiveValues(display=list(datasets::Orange,datasets::Orange))
  
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

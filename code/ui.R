gta_sql_pool_open()

addResourcePath(prefix = 'www', directoryPath = paste0(app.path,'www'))

ui <- fluidPage(
  useShinyjs(),
  theme = "www/style.css",
  tags$head(tags$link(rel="stylesheet", type="text/css", href="www/tipped.css")),
  tags$head(tags$script(src="www/tipped.js")),
  
  ######## DATA COUNTS #########
  tags$div(id="loading",
           tags$div(class="loading-background"),
           tags$div(class="img-holder",
                    tags$img(src="www/loading.svg"))),
  tags$div(class="header",
           tags$div(class="username",
                    textInput("username",
                              label=NULL,
                              placeholder = "Create User"),
                    actionButton("create.user",
                                 "Create"),
                    selectInput("users",
                                label=NULL,
                                choices = c("Choose User"="Select",c('kamran','piotr')),
                                multiple = F)),
           tags$div(class="logo",
                    img(src="ww/gta logo-white.svg"))),
  
  tags$div(class="overall-wrap",
           tags$div(class = "settings",
                    tags$div(class = "settings-inner",
                             tags$div(class="app-switcher",
                                      tags$div(class="tab-nav-wrapper",  
                                               tags$ul(class="tab-list nav",
                                                       tags$li(class="active check-suggestions",
                                                               HTML("<a data-toggle='tab' href='#term'>Tariff<br/>Search Settings</a>")
                                                       )))),
                             tags$div(class="settings-bottom",
                                      tags$div(class="scroll",
                                               tags$div(class="suggestions",
                                                        textAreaInput("hs.codes",
                                                                      label = "HS code input (csv).",
                                                                      value = "20110,020120,201,410419211,71021,71022,71029,71039,51110,50800",
                                                                      width = '100%',
                                                                      rows = 2)),
                                               tags$div(class="suggestions",
                                                        textAreaInput("cpc.codes",
                                                                      label = "CPC code input (csv).",
                                                                      value = '64',
                                                                      width = '100%',
                                                                      rows = 2)),
                                               tags$div(class="suggestions",
                                                        pickerInput("implementing.jurisdiction",
                                                                    "Implementing jurisdiction input",
                                                                    choices = c('Any',
                                                                                as.character(unique(country.names$name))),
                                                                    selected = as.character(unique(country.names$name))[c(1,5,15,25,50,100,150)],
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple =T)),
                                               tags$div(class="suggestions",
                                                        pickerInput("affected.jurisdiction",
                                                                    "Affected jurisdiction input",
                                                                    choices = c('Any',
                                                                                as.character(unique(country.names$name))),
                                                                    selected = 'Any',
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple =T)),
                                               tags$div(class="suggestions",
                                                        dateInput("start.date",
                                                                  "Start date",
                                                                  format='yyyy-mm-dd',
                                                                  value='2008-02-01')),
                                               tags$div(class="suggestions",
                                                        dateInput("end.date",
                                                                  "End date",
                                                                  format='yyyy-mm-dd',
                                                                  value='2016-02-01')),
                                               tags$div(class="suggestions",
                                                        pickerInput("regime.type",
                                                                    "Regime type input",
                                                                    choices = c('Any',
                                                                                'MFN','GSP','GSP+','LDC duties'),
                                                                    selected = 'Any',
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple =T)),
                                               tags$div(class="suggestions",
                                                        pickerInput("regime.name",
                                                                    "Regime name input",
                                                                    choices = c('Any',
                                                                                gta_sql_get_value("SELECT DISTINCT regime_name FROM regime_list")$regime.name[-1]),
                                                                    selected = 'Any',
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple =T)),
                                               tags$div(class="suggestions",
                                                        pickerInput("tariff.unit",
                                                                    "Which tariff units do you wish to display?",
                                                                    choices = c('Any',
                                                                                'max',
                                                                                'mean',
                                                                                'min'),
                                                                                
                                                                    selected = 'Any',
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple =T)),
                                               tags$div(class="suggestions",
                                                        prettySwitch("display.previous.tariff",
                                                                     "Display previous tariff",
                                                                     status='primary')), 
                                               tags$div(class="suggestions",
                                                        prettySwitch("sort.liberalised.restricted",
                                                                     "Sort by liberalised/restricted/unknown(in case no prev known value)",
                                                                     status='primary'))#,
                                               # tags$div(class="suggestions",
                                               #          textInput("new.rate",
                                               #                        label = "New rate",
                                               #                        value = "")),
                                               # tags$div(class="suggestions",
                                               #          pickerInput("new.rate.type",
                                               #                    label = "New rate type",
                                               #                    choices = as.character(unique(prior.new$applied.value.unit)),
                                               #                    options = list(`actions-box` = TRUE),
                                               #                    multiple=F)),
                                               # tags$div(class="suggestions",
                                               #          dateInput("new.rate.date",
                                               #                    "new rate date",
                                               #                    format='yyyy-mm-dd',
                                               #                    value=Sys.Date()))
                                               )),
                                                
                                               tags$div(class="continue-button",
                                                                 actionButton("submit.query",
                                                                              "Submit query"))

                            
                                            
                    )),
  
  tags$div(class="content",
           actionButton('clear.xlsx', 'Clear xlsx input'),
           fileInput('excel.query', 'Choose xlsx file',
                     accept = c(".xlsx")
           ),
           prettySwitch("xlsx.sort",
                        "xlsx sort by restricted/liberalised",
                        status='primary',
                        value=T),
           downloadButton("dl", "Download"),
           tags$div(class="results",
                    HTML("<br/><br/><br/><br/>"),
                    # tags$style("table.dataTable tr.selected td, table.dataTable tr { background-color:#f9f9f9 !important; }
                    #                  table.dataTable tr.selected td, table.dataTable tr.selected { background-color:#f3f3f3 !important; }
                    #                  table.dataTable tr.selected td, table.dataTable tr.group { background-color:#e7e7e7 !important; cursor:pointer; } 
                    #                  table.dataTable tr.selected td, table.dataTable td { background-color:transparent !important; }
                    #                  "),
                        uiOutput('sorted.table.outputs'),
                        uiOutput('table.outputs')
                    )
        )
  ),
  
  tags$script(src="tooltips.js")
  

                             )


gta_sql_pool_close()





























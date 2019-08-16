gta_sql_pool_open(table.prefix = 'delta_', got.keyring=F)

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
                    selectInput("users",
                                label=NULL,
                                choices = c("Choose User"="Select",
                                            gta_sql_get_value("SELECT DISTINCT `user_login` FROM `gta_user_log`")),
                                multiple = F)),
           tags$div(class="logo",
                    img(src="www/gta logo-white.svg"))),
  
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
                                      # tags$div(class="scroll",
                                               fluidRow(
                                               column(width=12,
                                               tags$div(class="suggestions",
                                                        textAreaInput("hs.codes",
                                                                      label = "HS code input (csv).",
                                                                      value = "20110",
                                                                      width = '395px',
                                                                      rows = 2)),
                                               column(width=6,
                                               fluidRow(        
                                               tags$div(class="suggestions",
                                                        pickerInput("implementing.jurisdiction",
                                                                    "implementer",
                                                                    choices = c('Any',
                                                                                as.character(unique(country.names$name))),
                                                                    selected = as.character(unique(country.names$name))[9],
                                                                    options = list(`actions-box` = TRUE))))),
                                               column(width=6,
                                               fluidRow(        
                                                        
                                               tags$div(class="suggestions",
                                                        pickerInput("affected.jurisdiction",
                                                                    "affected",
                                                                    choices = c('Any',
                                                                                as.character(unique(country.names$name))),
                                                                    selected = 'Any',
                                                                    options = list(`actions-box` = TRUE),
                                                                    multiple =T)))),
                                               column(width=12,
                                               fluidRow(        
                                               tags$div(class="suggestions",
                                                        dateInput("end.date",
                                                                  "End date",
                                                                  format='yyyy-mm-dd',
                                                                  value='2016-02-01',
                                                                  width='165px')))))
                                      )),
                             
                             tags$div(class="continue-button",
                                      actionButton("submit.query",
                                                   "Submit query"))
                             
                             
                             
                    )),
           
           
           tags$div(class="content",
                    tags$div(class="results",
                             uiOutput('display')
                    )
           )
           
           
  ),
  
  tags$script(src="tooltips.js")
  
  
)


gta_sql_pool_close()





























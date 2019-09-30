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
                                                               HTML("<a data-toggle='tab' href='#search'>Tariff<br/>Search Settings</a>")
                                                       ),
                                                       tags$li(class="upload",
                                                               HTML("<a data-toggle='tab' href='#upload'> Upload data </a>")
                                                       )))),
                             tags$div(class="settings-bottom",
                                      # tags$div(class="scroll",
                                    tags$div(class="tab-pane active fade in", id="search",
                                        column(width=12,
                                               column(width=6,
                                               tags$div(class="suggestions",
                                                        textAreaInput("treatment.codes",
                                                                      label = "Treatment code input (csv).",
                                                                      value = '900630,90240,10121',
                                                                      width = '200px',
                                                                      rows = 2))),
                                               column(width=5,
                                               tags$div(class="suggestions",
                                                        selectInput("treatment.code.type",
                                                                    "Treatment area",
                                                                    choices=c('hs','cpc'),
                                                                    selected='hs',
                                                                    width='70px'))),
                                               column(width=12,
                                                        tags$div(class="suggestions",
                                                                 multiInput("implementing.jurisdiction",
                                                                            "Restrict implementers",
                                                                            choices = gta_sql_get_value(paste('select distinct `jurisdiction_name` from gta_jurisdiction_list union',
                                                                                                              'select distinct `jurisdiction_group_name` from gta_jurisdiction_group_list')),
                                                                            selected = c('Angola','Argentina','Algeria','Albania')))),
                                               column(width=12,
                                                        tags$div(class="suggestions",
                                                                 multiInput("affected.jurisdiction",
                                                                            "Restrict affected countries",
                                                                            choices = c('MFN',gta_sql_get_value(paste('select distinct `jurisdiction_name` from gta_jurisdiction_list union',
                                                                                                              'select distinct `jurisdiction_group_name` from gta_jurisdiction_group_list'))),
                                                                            selected = 'MFN'))),
                                               column(width=12,
                                                        column(width=6,
                                                        tags$div(class="suggestions",
                                                                 dateInput("end.date",
                                                                           "End date",
                                                                           format='yyyy-mm-dd',
                                                                           value='2014-02-01',
                                                                           width='165px'))),
                                                        column(width=6,
                                                        tags$div(class="suggestions",
                                                                 selectInput("treatment.area",
                                                                           "Treatment area",
                                                                           choices=gta_sql_get_value('select distinct `treatment_area_name` from delta_treatment_area_list'),
                                                                           selected='tariff',
                                                                           width='165px'))),
                                                      hidden(actionButton('clear.xlsx', 'Clear xlsx input'))
                                               ),
                                               
                                               column(width=5,
                                                      tags$div(class="suggestions",
                                                               selectInput("affected.flow",
                                                                           "Affected flow",
                                                                           choices=gta_sql_get_value('select distinct `affected_flow` from gta_affected_flow_list'),
                                                                           selected=NULL,
                                                                           multiple=T,
                                                                           width='165px')))
                                               )
                                      ),
                                    tags$div(class="tab-pane fade", id="upload",
                                             column(width=12,
                                                    tags$div(class="suggestions",
                                                   tags$div(class="upload",
                                                            fileInput('excel.query', 'Choose xlsx or csv file (max 2gb)',
                                                                      accept = c(".xlsx",".csv"))
                                                   )
                                             ))
                                      
                                      ),
                             
                             tags$div(class="continue-button",
                                      column(width=12,
                                      actionButton("submit.query",
                                                   "Submit query")))
                             
                             
                             
                    ))),
           
           
           tags$div(class="content",
                    tags$div(class="results",
                             useShinyalert(),
                             uiOutput('display'),
                             dataTableOutput('unrecognized.variables')
                    )
           )
           
           
  ),
  
  tags$script(src="tooltips.js")
  
  
)


gta_sql_pool_close()




























